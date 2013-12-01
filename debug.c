#include "ast.h"

#if 0
static void debug_traverse(ast_function *fun) {
    ast_node    *at;
    size_t       depth;
    ast_iterator iter;
    ast_iterator_begin(&iter, (ast_node*)fun);

    for (at = (ast_node*)fun; at; at = ast_iterator_next(&iter)) {
        for (depth = vec_size(iter.path); depth; --depth)
            con_out("> ");
        con_out("ast_%s (%p)\n", ast_node_type_name[at->nodetype], at);
    }

    ast_iterator_delete(&iter);
}
#endif
