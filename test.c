/*
 * Copyright (C) 2012
 *     Dale Weiler
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#include "gmqcc.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

bool  opts_memchk = true;
bool  opts_debug  = false;
char *task_bins[] = {
    "./gmqcc",
    "./qcvm"
};

#define TASK_COMPILE 0
#define TASK_EXECUTE 1

/*
 * Task template system:
 *  templates are rules for a specific test, used to create a "task" that
 *  is executed with those set of rules (arguments, and what not). Tests
 *  that don't have a template with them cannot become tasks, since without
 *  the information for that test there is no way to properly "test" them.
 *  Rules for these templates are described in a template file, using a
 *  task template language.
 * 
 *  The language is a basic finite statemachine, top-down single-line
 *  description language.
 * 
 *  The languge is composed entierly of "tags" which describe a string of
 *  text for a task.  Think of it much like a configuration file.  Except
 *  it's been designed to allow flexibility and future support for prodecual
 *  semantics.
 * 
 *  The following "tags" are suported by the language
 * 
 *      D:
 *          Used to set a description of the current test, this must be
 *          provided, this tag is NOT optional.
 * 
 *      F:
 *          Used to set a failure message, this message will be displayed
 *          if the test fails, this tag is optional
 * 
 *      S:
 *          Used to set a success message, this message will be displayed
 *          if the test succeeds, this tag is optional.
 * 
 *      T:
 *          Used to set the procedure for the given task, there are two
 *          options for this:
 *              -compile
 *                  This simply performs compilation only
 *              -execute
 *                  This will perform compilation and execution
 * 
 *          This must be provided, this tag is NOT optional.
 * 
 *      C:
 *          Used to set the compilation flags for the given task, this
 *          must be provided, this tag is NOT optional.
 * 
 *      E:
 *          Used to set the execution flags for the given task. This tag
 *          must be provided if T == -execute, otherwise it's erroneous
 *          as compilation only takes place. 
 *      
 *      M:
 *          Used to describe a string of text that should be matched from
 *          the output of executing the task.  If this doesn't match the
 *          task fails.  This tag must be provided if T == -execute, otherwise
 *          it's erroneous as compilation only takes place.
 * 
 *      I:
 *          Used to specify the INPUT source file to operate on, this must be
 *          provided, this tag is NOT optional.
 * 
 *  Notes:
 *      These tags have one-time use, using them more than once will result
 *      in template compilation errors.
 * 
 *      Lines beginning with # or // in the template file are comments and
 *      are ignored by the template parser.
 * 
 *      Whitespace is optional, with exception to the colon ':' between the
 *      tag and it's assignment value/
 * 
 *      The template compiler will detect erronrous tags (optional tags
 *      that need not be set), as well as missing tags, and error accordingly
 *      this will result in the task failing.
 */
typedef struct {
    char *description;
    char *failuremessage;
    char *successmessage;
    char *compileflags;
    char *executeflags;
    char *comparematch;
    char *proceduretype;
    char *sourcefile;
    char *tempfilename;
} task_template_t;

/*
 * This is very much like a compiler code generator :-).  This generates
 * a value from some data observed from the compiler.
 */
bool task_template_generate(task_template_t *template, char tag, const char *file, size_t line, const char *value) {
    char **destval = NULL;
    
    if (!template)
        return false;
    
    switch(tag) {
        case 'D': destval = &template->description;    break;
        case 'F': destval = &template->failuremessage; break;
        case 'S': destval = &template->successmessage; break;
        case 'T': destval = &template->proceduretype;  break;
        case 'C': destval = &template->compileflags;   break;
        case 'E': destval = &template->executeflags;   break;
        case 'M': destval = &template->comparematch;   break;
        case 'I': destval = &template->sourcefile;     break;
        default:
            con_printmsg(LVL_ERROR, __FILE__, __LINE__, "internal error",
                "invalid tag `%c:` during code generation\n",
                tag
            );
            return false;
    }
    
    /*
     * Ensure if for the given tag, there already exists a
     * assigned value.
     */
    if (*destval) {
        con_printmsg(LVL_ERROR, file, line, "compile error",
            "tag `%c:` already assigned value: %s\n",
            tag, *destval
        );
        return false;
    }
    
    /*
     * Strip any whitespace that might exist in the value for assignments
     * like "D:      foo"
     */
    if (value && *value && (*value == ' ' || *value == '\t'))
        value++;
    
    /*
     * Value will contain a newline character at the end, we need to strip
     * this otherwise kaboom, seriously, kaboom :P
     */
    *strrchr(value, '\n')='\0';
    
    /*
     * Now allocate and set the actual value for the specific tag. Which
     * was properly selected and can be accessed with *destval.
     */
    *destval = util_strdup(value);
    
    return true;
}

bool task_template_parse(const char *file, task_template_t *template, FILE *fp) {
    char  *data = NULL;
    char  *back = NULL;
    size_t size = 0;
    size_t line = 1;
    
    if (!template)
        return false;
    
    /* top down parsing */
    while (util_getline(&back, &size, fp) != EOF) {
        /* skip whitespace */
        data = back;
        if (*data && (*data == ' ' || *data == '\t'))
            data++;
            
        switch (*data) {
            /*
             * Handle comments inside task template files.  We're strict
             * about the language for fun :-)
             */
            case '/':
                if (data[1] != '/') {
                    con_printmsg(LVL_ERROR, file, line, "template parse error",
                        "invalid character `/`, perhaps you meant `//` ?");
                    
                    mem_d(back);
                    return false;
                }
            case '#':
                break;
                
            /*
             * Empty newlines are acceptable as well, so we handle that here
             * despite being just odd since there should't be that many
             * empty lines to begin with.
             */
            case '\r':
            case '\n':
                break;
                
                
            /*
             * Now begin the actual "tag" stuff.  This works as you expect
             * it to.
             */
            case 'D':
            case 'F':
            case 'S':
            case 'T':
            case 'C':
            case 'E':
            case 'M':
            case 'I':
                if (data[1] != ':') {
                    con_printmsg(LVL_ERROR, file, line, "template parse error",
                        "expected `:` after `%c`",
                        *data
                    );
                    goto failure;
                }
                if (!task_template_generate(template, *data, file, line, &data[3])) {
                    con_printmsg(LVL_ERROR, file, line, "template compile error",
                        "failed to generate for given task\n"
                    );
                    goto failure;
                }
                break;
            
            default:
                con_printmsg(LVL_ERROR, file, line, "template parse error",
                    "invalid tag `%c`", *data
                );
                goto failure;
            /* no break required */
        }
        
        /* update line and free old sata */
        line++;
        mem_d(back);
        back = NULL;
    }
    if (back)
        mem_d(back);
    return true;
    
failure:
    if (back)
        mem_d (back);
    return false;
}

/*
 * Nullifies the template data: used during initialization of a new
 * template and free.
 */
void task_template_nullify(task_template_t *template) {
    if (!template)
        return;
        
    template->description    = NULL;
    template->failuremessage = NULL;
    template->successmessage = NULL;
    template->proceduretype  = NULL;
    template->compileflags   = NULL;
    template->executeflags   = NULL;
    template->comparematch   = NULL;
    template->sourcefile     = NULL;
    template->tempfilename   = NULL;
}

task_template_t *task_template_compile(const char *file, const char *dir) {
    /* a page should be enough */
    char             fullfile[4096];
    FILE            *tempfile = NULL;
    task_template_t *template = NULL;
    
    memset  (fullfile, 0, sizeof(fullfile));
    snprintf(fullfile,    sizeof(fullfile), "%s/%s", dir, file);
    
    tempfile = fopen(fullfile, "r");
    template = mem_a(sizeof(task_template_t));
    task_template_nullify(template);
    
    /*
     * Esnure the file even exists for the task, this is pretty useless
     * to even do.
     */
    if (!tempfile) {
        con_err("template file: %s does not exist or invalid permissions\n",
            file
        );
        goto failure;
    }
    
    if (!task_template_parse(file, template, tempfile)) {
        con_err("template parse error: error during parsing\n");
        goto failure;
    }
    
    /*
     * Regardless procedure type, the following tags must exist:
     *  D
     *  T
     *  C
     *  I
     */
    if (!template->description) {
        con_err("template compile error: %s missing `D:` tag\n", file);
        goto failure;
    }
    if (!template->proceduretype) {
        con_err("template compile error: %s missing `T:` tag\n", file);
        goto failure;
    }
    if (!template->compileflags) {
        con_err("template compile error: %s missing `C:` tag\n", file);
        goto failure;
    }
    if (!template->sourcefile) {
        con_err("template compile error: %s missing `I:` tag\n", file);
        goto failure;
    }
    
    /*
     * Now lets compile the template, compilation is really just
     * the process of validating the input.
     */
    if (!strcmp(template->proceduretype, "-compile")) {
        if (template->executeflags)
            con_err("template compile warning: %s erroneous tag `E:` when only compiling\n", file);
        if (template->comparematch)
            con_err("template compile warning: %s erroneous tag `M:` when only compiling\n", file);
        goto success;
    } else if (!strcmp(template->proceduretype, "-execute")) {
        if (!template->executeflags) {
            con_err("template compile error: %s missing `E:` tag (use `$null` for exclude)\n", file);
            goto failure;
        }
        if (!template->comparematch) {
            con_err("template compile error: %s missing `M:` tag (use `$null` for exclude)\n", file);
            goto failure;
        }
    } else {
        con_err("template compile error: %s invalid procedure type: %s\n", file, template->proceduretype);
        goto failure;
    }
    
success:
    fclose(tempfile);
    return template;
    
failure:
    /*
     * The file might not exist and we jump here when that doesn't happen
     * so the check to see if it's not null here is required.
     */
    if (tempfile)
        fclose(tempfile);
    mem_d (template);
    
    return NULL;
}

void task_template_destroy(task_template_t **template) {
    if (!template)
        return;
        
    if ((*template)->description)    mem_d((*template)->description);
    if ((*template)->failuremessage) mem_d((*template)->failuremessage);
    if ((*template)->successmessage) mem_d((*template)->successmessage);
    if ((*template)->proceduretype)  mem_d((*template)->proceduretype);
    if ((*template)->compileflags)   mem_d((*template)->compileflags);
    if ((*template)->executeflags)   mem_d((*template)->executeflags);
    if ((*template)->comparematch)   mem_d((*template)->comparematch);
    if ((*template)->sourcefile)     mem_d((*template)->sourcefile);
    
    /*
     * Nullify all the template members otherwise NULL comparision
     * checks will fail if template pointer is reused.
     */
    mem_d(*template);
    task_template_nullify(*template);
    *template = NULL;
}

/*
 * Now comes the task manager, this system allows adding tasks in and out
 * of a task list.  This is the executor of the tasks essentially as well.
 */
typedef struct {
    task_template_t *template;
    FILE            *handle;
} task_t;

task_t *task_tasks = NULL;

/*
 * Read a directory and searches for all template files in it
 * which is later used to run all tests.
 */
bool task_propogate(const char *curdir) {
    bool             success = true;
    DIR             *dir;
    struct dirent   *files;
    struct stat      directory;
    char             buffer[4096];
    
    dir = opendir(curdir);
    
    while ((files = readdir(dir))) {
        memset  (buffer, 0,sizeof(buffer));
        snprintf(buffer,   sizeof(buffer), "%s/%s", curdir, files->d_name);
        
        if (stat(buffer, &directory) == -1) {
            con_err("internal error: stat failed, aborting\n");
            abort();
        }
        
        /* skip directories */
        if (S_ISDIR(directory.st_mode))
            continue;
            
        /*
         * We made it here, which concludes the file/directory is not
         * actually a directory, so it must be a file :)
         */
        if (strstr(files->d_name, ".tmpl")) {
            con_out("compiling task template: %s/%s\n", curdir, files->d_name);
            task_template_t *template = task_template_compile(files->d_name, curdir);
            if (!template) {
                con_err("error compiling task template: %s\n", files->d_name);
                success = false;
                continue;
            }
            /*
             * Generate a temportary file name for the output binary
             * so we don't trample over an existing one.
             */
            template->tempfilename = tempnam(curdir, "TMPDAT");
            
            
            /*
             * Generate the command required to open a pipe to a process
             * which will be refered to with a handle in the task for
             * reading the data from the pipe.
             */
            char     buf[4096]; /* one page should be enough */
            memset  (buf,0,sizeof(buf));
            snprintf(buf,  sizeof(buf), "%s %s/%s %s -o %s",
                task_bins[TASK_COMPILE],
                curdir,
                template->sourcefile,
                template->compileflags,
                template->tempfilename
            );
            
            /*
             * The task template was compiled, now lets create a task from
             * the template data which has now been propogated.
             */
            task_t task;
            task.template = template;
            if (!(task.handle = popen(buf, "r"))) {
                con_err("error opening pipe to process for test: %s\n", template->description);
                success = false;
                continue;
            }
            con_out("executing test: `%s` [%s]\n", template->description, buf);
            
            vec_push(task_tasks, task);
        }
    }
    
    closedir(dir);
    return success;
}

/*
 * Removes all temporary 'progs.dat' files created during compilation
 * of all tests'
 */
void task_cleanup(const char *curdir) {
    DIR             *dir;
    struct dirent   *files;
    char             buffer[4096];

    dir = opendir(curdir);
    
    while ((files = readdir(dir))) {
        memset(buffer, 0, sizeof(buffer));
        if (strstr(files->d_name, "TMP")) {
            snprintf(buffer, sizeof(buffer), "%s/%s", curdir, files->d_name);
            if (remove(buffer))
                con_err("error removing temporary file: %s\n", buffer);
            else
                con_out("removed temporary file: %s\n", buffer);
        }
    }
    
    closedir(dir);
}

void task_destroy(const char *curdir) {
    /*
     * Free all the data in the task list and finally the list itself
     * then proceed to cleanup anything else outside the program like
     * temporary files.
     */
    size_t i;
    for (i = 0; i < vec_size(task_tasks); i++)
        task_template_destroy(&task_tasks[i].template);
    vec_free(task_tasks);
    
    /*
     * Cleanup outside stuff like temporary files.
     */
    task_cleanup(curdir);
}

/*
 * This executes the QCVM task for a specificly compiled progs.dat
 * using the template passed into it for call-flags and user defined
 * messages.
 */
bool task_execute(task_template_t *template) {
    bool     success = false;
    FILE    *execute;
    char     buffer[4096];
    memset  (buffer,0,sizeof(buffer));
    
    /*
     * Drop the execution flags for the QCVM if none where
     * actually specified.
     */
    if (!strcmp(template->executeflags, "$null")) {
        snprintf(buffer,  sizeof(buffer), "%s %s",
            task_bins[TASK_EXECUTE],
            template->tempfilename
        );
    } else {
        snprintf(buffer,  sizeof(buffer), "%s %s %s",
            task_bins[TASK_EXECUTE],
            template->executeflags,
            template->tempfilename
        );
    }
    
    con_out("executing qcvm: `%s` [%s]\n",
        template->description,
        buffer
    );
    
    execute = popen(buffer, "r");
    if (!execute)
        return false;
        
    /*
     * Now lets read the lines and compare them to the matches we expect
     * and handle accordingly.
     */
    {
        char  *data  = NULL;
        size_t size  = 0;
        while (util_getline(&data, &size, execute) != EOF) {}
        
        if (!strcmp(data, "No main function found\n")) {
            con_err("test failure: `%s` [%s] (No main function found)\n",
                template->description,
                (template->failuremessage) ?
                template->failuremessage : "unknown"
            );
            pclose(execute);
            return false;
        }
        
        /* 
         * Trim newlines from data since they will just break our
         * ability to properly validate matches.
         */
        if  (strrchr(data, '\n'))
            *strrchr(data, '\n') = '\0';
        
        /* null list */
        if (!strcmp(template->comparematch, "$null"))
            success = true;
        
        /*
         * We only care about the last line from the output for now
         * implementing multi-line match is TODO.
         */
        if (!strcmp(data, template->comparematch))
            success = true;
    }
    pclose(execute);
    return success;
}

/*
 * This schedualizes all tasks and actually runs them individually
 * this is generally easy for just -compile variants.  For compile and
 * execution this takes more work since a task needs to be generated
 * from thin air and executed INLINE.
 */
void task_schedualize(const char *curdir) {
    bool   execute = false;
    char  *back    = NULL;
    char  *data    = NULL;
    size_t size    = 0;
    size_t i;
    
    for (i = 0; i < vec_size(task_tasks); i++) {
        /*
        * Generate a task from thin air if it requires execution in
        * the QCVM.
        */
        if (!strcmp(task_tasks[i].template->proceduretype, "-execute"))
            execute = true;
            
        while (util_getline(&data, &size, task_tasks[i].handle) != EOF) {
            back = data;
            /* chances are we want to print errors */
            if (strstr(data, "error")) {
                con_out("compile failed: %s\n",
                    /* strip the newline from the end */
                    (*strrchr(data, '\n')='\0')
                );
                
                /*
                 * The compilation failed which means it cannot be executed
                 * as the file simply will not exist.
                 */
                execute = false;
                break;
            }
        }
        if (back)
            mem_d(back);
        
        /*
         * If we can execute we do so after all data has been read and
         * this paticular task has coupled execution in its procedure type
         */
        if (!execute)
            continue;
        
        /*
         * If we made it here that concludes the task is to be executed
         * in the virtual machine.
         */
        if (!task_execute(task_tasks[i].template)) {
            con_err("test failure: `%s` [%s]\n",
                task_tasks[i].template->description,
                (task_tasks[i].template->failuremessage) ?
                task_tasks[i].template->failuremessage : "unknown"
            );
            continue;
        }
        
        con_out("test succeed: `%s` [%s]\n",
            task_tasks[i].template->description,
            (task_tasks[i].template->successmessage) ?
            task_tasks[i].template->successmessage  : "unknown"
        );
    }
    if (back)
        mem_d(back);
}

int main(int argc, char **argv) {
    con_init();
    if (!task_propogate("tests")) {
        con_err("error: failed to propogate tasks\n");
        task_destroy("tests");
        return -1;
    }
    /*
     * If we made it here all tasks where propogated from their resultant
     * template file.  So we can start the FILO scheduler, this has been
     * designed in the most thread-safe way possible for future threading
     * it's designed to prevent lock contention, and possible syncronization
     * issues.
     */
    task_schedualize("tests");
    task_destroy("tests");
    
    util_meminfo();
    return 0;
}
