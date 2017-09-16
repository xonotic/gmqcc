#include "parser.h"

#include <cstring>
#include <map>


/*
 * have a tape stack for pushing assigning CST nodes to as they are completed
 * have a templated destructor wrapper serving double duty as a destructor + union discriminant
 * reset the stack on error, destroying everything as needed
 * on completion of some nodes (if, while, for, etc -- probably everything but expressions)
 * take all prior parameters and produce some kind of AST node
 * is there a way to convert from a pointer to a destructor impl to its size? need a size for traversing the stack
 *
 */

template<class T>
void destructor(void *v) {
    reinterpret_cast<T *>(v)->~T();
}

class stack_t {
    using byte = std::uint8_t;
    using destructor_t = void (&)(void *it);

    struct entry_t {
        stack_t *owner;
        std::size_t size;
        std::size_t offset;
        destructor_t dtor;

        entry_t() : entry_t(nullptr, static_cast<std::size_t>(0), static_cast<std::size_t>(0), destructor<nullptr_t>) {}

        entry_t(stack_t *owner, std::size_t size, std::size_t offset, destructor_t dtor) : owner(owner),
                                                                                                 size(size),
                                                                                                 offset(offset),
                                                                                                 dtor(dtor) {}

        ~entry_t() {
            auto self = this;
            printf("%p\n", self);
            auto &stack = owner->stack;
            auto ptr = &stack[offset];
            printf("DELETE AT %p with %p\n", &stack[offset], dtor);
            dtor(reinterpret_cast<void *>(ptr));
        }
    };

    std::vector<entry_t> entries;
    std::vector<byte> stack;

public:

    using size_t = struct {
        size_t entries;
        size_t stack;
    };

    size_t size() {
        return {entries.size(), stack.size()};
    }

    std::size_t sizeat(std::size_t i) {
        return entries[i].size;
    }

    void resize(size_t sp) {
        entries.resize(sp.entries);
        stack.resize(sp.stack);
    }

    template<class T>
    void push(T &&it) {
        auto size = sizeof(T);
        auto offset = stack.size();
        stack.resize(offset + size);
        new(&stack[offset]) T(std::move(it));
        auto &dtor = destructor<T>;
        printf("MADE AT %p with %p\n", &stack[offset], dtor);
        entries.emplace_back(this, size, offset, dtor);
    }

    void pop() {
        auto size = entries.back().size;
        entries.pop_back();
        stack.resize(stack.size() - size);
    }

};

template<class T, class U = T>
T exchange(T &obj, U &&new_value) {
    T old_value = std::move(obj);
    obj = std::forward<U>(new_value);
    return old_value;
}

//void never() {
//    struct test {
//        std::uint8_t okay;
//        std::uint8_t *x;
//
//        test() {
//            this->x = new std::uint8_t;
//            this->okay = 47;
//            printf("allocated %p\n", this->x);
//        }
//
//        test(test &&o) : x(exchange(o.x, nullptr)), okay(o.okay) {
//            printf("stole from %p into %p\n", &o, this);
//        }
//
//        ~test() {
//            printf("bye %p\n", this);
//            delete this->x;
//        }
//    };
//    auto d2 = destructor<test>;
//    printf("dtor %p\n", d2);
//    {
////        auto it = test{};
////        printf("pushing: %p\n", &it);
////        stack.push(std::move(it));
////        stack.pop();
//    }
//    printf("over\n");
//}

// fixme: register typedef'd types. this wouldn't matter if parameters required names
//        defeated by a mere for (i = 0; i < n; ++i)
//        `for\s*\(\s*([^\s]+)\s*=` -> `for (auto $1 =`
// fixme: GenericCommand_rpn  --  operators in strings??? specifically "="??
// fixme: HUD_ItemsTime
// todo: template traits for each rule to enable prediction instead of backtracking
// todo: commit to a rule if possible and disable backtracking
// todo: store memo on heap
// todo: parameterize expression parsing instead of separate rules

#define RVALUE(it) ((void) 0, (it))

namespace result {

#define OK() return Result::OK
#define ERR(msg) return Result((std::string("") + __PRETTY_FUNCTION__ + ": " + msg), ctx.tok)

    struct Result {
        static Result OK;

        explicit Result(std::string error, Token t) : tok(t) {
            auto tokname = std::string(TokenName(t));
            auto tokval = std::to_string(t);
            this->error = std::move(error) + " -> " + tokname + " (" + tokval + ")";
        }

        std::string error;
        Token tok;

        operator bool() const {
            return Result::OK == *this;
        }

        bool operator==(const Result &other) const {
            return error == other.error;
        }
    };

    Result Result::OK = Result("", Token::NONE);
}

using Result = result::Result;

/// lexer

#define LTOK() RVALUE(ctx.lex.tok)
#define CTX() RVALUE(LTOK().ctx)

#define SOURCELOC() \
    RVALUE((std::string(CTX().file) + ":" + std::to_string(CTX().line) + ":" + std::to_string(CTX().column)).c_str())

#define STRING() RVALUE((LTOK().value).c_str())
#define DEBUG(msg) RVALUE(ctx.debug((std::string("*** `") + STRING() + "`: " + msg).c_str()))

/// parser

struct memo_t {
    decltype(lex_file::tok) tok;
    /// XXX: hack
    decltype(lex_file::peek) peek;
    size_t peekpos;
    size_t line, column;
    size_t idx;
    stack_t::size_t sp;
};

struct ctx_t {
    parser_t &parser;
    lex_file &lex;
    Token tok;

    stack_t stack = {};

    std::map<std::string, int> typedefs;

    explicit ctx_t(parser_t &parser) : parser(parser), lex(*parser.lex) {
        tok = Token::NONE;
    }

    memo_t memo() {
        auto idx = lex.file ? ftell(lex.file) : lex.open_string_pos;
        return memo_t{lex.tok, lex.peek, lex.peekpos, lex.line, lex.column, idx, stack.size()};
    }

    void memo(memo_t memo) {
        lex.tok = memo.tok;
        tok = memo.tok.ttype;
        lex.peek = memo.peek;
        lex.peekpos = memo.peekpos;
        lex.line = memo.line;
        lex.column = memo.column;
        if (lex.file) {
            fseek(lex.file, memo.idx, SEEK_SET);
        } else if (lex.open_string) {
            lex.open_string_pos = memo.idx;
        }
        stack.resize(memo.sp);
    }

    void next() {
        tok = lex_do(parser.lex);
        if (tok >= Token::ERROR) {
            error("lex error");
            tok = Token::NONE;
        }
    }

    template<typename... Ts>
    inline void error(const char *fmt, const Ts &...ts) {
        return parseerror(parser, fmt, formatNormalize(ts)...);
    }

    std::string indent = "";

    void rule_enter(const char *rule) {
        auto &ctx = *this;
        debug((std::string(rule) + " : `" + STRING() + "`").c_str());
        indent += "  ";
    }

    void rule_leave(const char *rule, Result &ret) {
        indent.resize(std::max(static_cast<size_t>(0), indent.size() - 2));
        if (ret) {
            debug((std::string(rule) + "::OK").c_str());
        } else {
            debug((std::string(rule) + "::ERR").c_str());
        }
    }

    void debug(const char *msg) {
        printf("%s%s\n", indent.c_str(), msg);
    }

private:
    static void parseerror(parser_t &parser, const char *fmt, ...) {
        va_list ap;
        va_start(ap, fmt);
        vcompile_error(parser.lex->tok.ctx, fmt, ap);
        va_end(ap);
    }
};

// entrypoint

static bool parser_compile(ctx_t &&ctx);

using Rule = Result(ctx_t &ctx);

namespace parse {

    Result dummy_result() {
        return Result::OK;
    }

    template<class G, class R>
    auto rule_do(ctx_t &ctx) -> decltype(R::act(ctx, {}, {}, static_cast<G *>(nullptr), 0), dummy_result()) {
        auto begin = ctx.stack.size();
        ctx.rule_enter(R::name);
        auto ret = R::call(ctx);
        ctx.rule_leave(R::name, ret);
        if (ret) {
            auto end = ctx.stack.size();
            end.stack -= ctx.stack.sizeat(end.entries -= 1);
            R::act(ctx, begin, end, static_cast<G *>(nullptr), 0);
        }
        return ret;
    }

#define RULE(rule) \
struct rule##_traits { \
    static constexpr const char *name = #rule; \
    static Result call(ctx_t &ctx) { return impl_##rule##_fn(ctx); } \
    template<class G> static auto act(ctx_t &ctx, stack_t::size_t begin, stack_t::size_t end, G*, int) -> decltype(G::act_##rule(ctx, begin, end), void()) { G::act_##rule(ctx, begin, end); } \
    template<class G> static auto act(ctx_t&, stack_t::size_t, stack_t::size_t, G*, long) -> decltype(void()) { } \
}; \
static Result rule(ctx_t &ctx) { return parse::rule_do<grammar, rule##_traits>(ctx); } \
static Result impl_##rule##_fn(ctx_t &ctx)

#define ACTION(rule) \
static void act_##rule(ctx_t &ctx, stack_t::size_t begin, stack_t::size_t end)

#define TRY(...) do { \
    auto ret = __VA_ARGS__; \
    if (!ret) { \
        return ret; \
    } \
} while (false)

#define PEEK() RVALUE(ctx.tok)
#define ACCEPT(it) RVALUE(parse::accept(ctx, (it)))

    bool accept(ctx_t &ctx, Token t) {
        if (PEEK() == t) {
            ctx.next();
            return true;
        }
        return false;
    }

#define EXPECT(tok) EXPECT_2(tok, "unexpected symbol, " + TokenName(tok) + " was expected")
#define EXPECT_2(tok, msg) do { \
    if (!ACCEPT(tok)) ERR(msg); \
} while (false)


#define ACCEPT_IDENT(it) (parse::accept_ident(ctx, (it)))

    bool accept_ident(ctx_t &ctx, const char *ident) {
        if (PEEK() == Token::IDENT && strcmp(STRING(), ident) == 0) {
            ctx.next();
            return true;
        }
        return false;
    }
}

namespace utils {

#define BT() ERR("BT")

/// rule[0] rule[1..n]
template<Rule... rules>
Result seq(ctx_t &ctx) {
    auto list = {rules...};
    for (auto &rule : list) {
        TRY(rule(ctx));
    }
    OK();
}

/// rule?
#define OPT(...) ([&](ctx_t &) { \
    auto memo = ctx.memo(); \
    if (!__VA_ARGS__(ctx)) ctx.memo(memo); \
    OK(); \
})

/// rule?
template<Rule rule>
Result opt(ctx_t &ctx) {
    return OPT(rule)(ctx);
}

/// rule+
#define CROSS(...) ([&](ctx_t &) { \
    TRY(__VA_ARGS__(ctx)); \
    for (;;) { \
        auto memo = ctx.memo(); \
        if (!__VA_ARGS__(ctx)) { \
            ctx.memo(memo); \
            break; \
        } \
    } \
    OK(); \
})

/// rule+
template<Rule rule>
Result cross(ctx_t &ctx) {
    return CROSS(rule)(ctx);
}

/// rule* == (rule+)?
#define STAR(...) OPT(CROSS(__VA_ARGS__))

/// rule* == (rule+)?
template<Rule rule>
Result star(ctx_t &ctx) {
    return STAR(rule)(ctx);
}

/// rule (s rule)*
template<Rule rule, Rule s>
Result sep(ctx_t &ctx) {
    TRY(rule(ctx));
    TRY(STAR(seq<s, rule>)(ctx));
    OK();
}

/// rule[0] | rule[1..n]
#define ALT(...) ([&](ctx_t &) { \
    auto memo = ctx.memo(); \
    auto list = {__VA_ARGS__}; \
    for (auto &rule : list) { \
        auto ret = rule(ctx); \
        if (ret) \
            OK(); \
        ctx.memo(memo); \
    } \
    BT(); \
})

/// rule[0] | rule[1..n]
template<Rule... rules>
Result alt(ctx_t &ctx) {
    return ALT(rules...)(ctx);
}

template<Token t>
Result tok(ctx_t &ctx) {
    EXPECT(t);
    OK();
}

template<Token t, char... chars>
Result lit(ctx_t &ctx) {
    static char str_[] = {chars..., 0};
    const char *str = str_;
    if (PEEK() == t) {
        const char *s = STRING();
        auto diff = strcmp(str, s);
        ctx.next();
        if (!diff) OK();
    }
    BT();
}

template<Rule next, Rule op>
Result leftop(ctx_t &ctx) {
    return sep<next, op>(ctx);
}

template<Rule rule, class Then>
Result when(ctx_t &ctx) {
    auto ret = rule(ctx);
    if (ret) {
        (*static_cast<Then *>(nullptr))(ctx);
    }
    return ret;
}

}

using namespace utils;

struct grammar {

    static constexpr auto Void = lit<Token::TYPENAME, 'v', 'o', 'i', 'd'>;
    static constexpr auto Char = lit<Token::TYPENAME, 'c', 'h', 'a', 'r'>;
    static constexpr auto Int = lit<Token::TYPENAME, 'i', 'n', 't'>;
    static constexpr auto Float = lit<Token::TYPENAME, 'f', 'l', 'o', 'a', 't'>;
    static constexpr auto Vector = lit<Token::TYPENAME, 'v', 'e', 'c', 't', 'o', 'r'>;
    static constexpr auto String = lit<Token::TYPENAME, 's', 't', 'r', 'i', 'n', 'g'>;
    static constexpr auto Entity = lit<Token::TYPENAME, 'e', 'n', 't', 'i', 't', 'y'>;

    static constexpr auto Enum = lit<Token::IDENT, 'e', 'n', 'u', 'm'>;
    static constexpr auto Typedef = lit<Token::IDENT, 't', 'y', 'p', 'e', 'd', 'e', 'f'>;

    static constexpr auto Const = lit<Token::KEYWORD, 'c', 'o', 'n', 's', 't'>;
    static constexpr auto Extern = lit<Token::KEYWORD, 'e', 'x', 't', 'e', 'r', 'n'>;
    static constexpr auto Static = lit<Token::IDENT, 's', 't', 'a', 't', 'i', 'c'>;
    static constexpr auto Noref = lit<Token::IDENT, 'n', 'o', 'r', 'e', 'f'>;
    static constexpr auto Local = lit<Token::KEYWORD, 'l', 'o', 'c', 'a', 'l'>;
    static constexpr auto Var = lit<Token::IDENT, 'v', 'a', 'r'>;

    static constexpr auto If = lit<Token::KEYWORD, 'i', 'f'>;
    static constexpr auto Else = lit<Token::KEYWORD, 'e', 'l', 's', 'e'>;
    static constexpr auto Switch = lit<Token::IDENT, 's', 'w', 'i', 't', 'c', 'h'>;
    static constexpr auto Case = lit<Token::IDENT, 'c', 'a', 's', 'e'>;
    static constexpr auto Default = lit<Token::KEYWORD, 'd', 'e', 'f', 'a', 'u', 'l', 't'>;

    static constexpr auto While = lit<Token::KEYWORD, 'w', 'h', 'i', 'l', 'e'>;
    static constexpr auto Do = lit<Token::KEYWORD, 'd', 'o'>;
    static constexpr auto For = lit<Token::KEYWORD, 'f', 'o', 'r'>;

    static constexpr auto Goto = lit<Token::IDENT, 'g', 'o', 't', 'o'>;
    static constexpr auto Continue = lit<Token::KEYWORD, 'c', 'o', 'n', 't', 'i', 'n', 'u', 'e'>;
    static constexpr auto Break = lit<Token::KEYWORD, 'b', 'r', 'e', 'a', 'k'>;
    static constexpr auto Return = lit<Token::KEYWORD, 'r', 'e', 't', 'u', 'r', 'n'>;

    // declarations

    /// : translationUnit? EOF
    RULE(compilationUnit) {
        TRY(OPT(translationUnit)(ctx));
        TRY(tok<Token::END>(ctx));
        OK();
    }

    ACTION(compilationUnit) {
        printf("DONE!\n");
    }

    /// : externalDeclaration+
    RULE(translationUnit) {
        TRY(CROSS(externalDeclaration)(ctx));
        OK();
    }

    struct externalDeclaration_declaration {
    };

    /// : pragma
    /// | functionDefinition
    /// | declaration
    /// | enumDeclaration
    /// | ';'
    RULE(externalDeclaration) {
        TRY(alt<
                pragma,
                functionDefinition,
                declaration,
                enumDeclaration,
                tok<Token::SEMICOLON>
        >(ctx));
        OK();
    }

    ACTION(declaration) {
        printf("START %d\n", begin);
        ctx.stack.push(externalDeclaration_declaration{});
    }

    ACTION(externalDeclaration) {

        printf("FINISH %d\n", begin);
    }

    RULE(pragma) {
        EXPECT(Token::HASH);
        if (ACCEPT_IDENT("pragma")) {
            if (ACCEPT_IDENT("noref")) {
                EXPECT(Token::INTCONST);
                OK();
            }
            ERR("unknown pragma '" + STRING() + "'");
        }
        ERR("unknown pragma '" + STRING() + "'");
    }

    /// : declarationSpecifiers? declarator compoundStatement
    /// | declarationSpecifiers? declarator '=' compoundStatement  # legacy
    RULE(functionDefinition) {
        TRY(OPT(declarationSpecifiers)(ctx));
        TRY(declarator(ctx));
        TRY(OPT(tok<Token::EQ>)(ctx)); // legacy
        TRY(compoundStatement(ctx));
        OK();
    }

    /// : attribute* (storageClassSpecifier | typeQualifier)* typeSpecifier
    RULE(declarationSpecifiers) {
        TRY(STAR(attribute)(ctx));
        TRY(STAR(alt<storageClassSpecifier, typeQualifier>)(ctx));
        TRY(typeSpecifier(ctx));
        OK();
    }

    struct attribute_alias {
    };

    struct attribute_eraseable {
    };

    struct attribute_accumulate {
    };

    /// : '[[' X ']]'
    RULE(attribute) {
        EXPECT(Token::ATTRIBUTE_OPEN);
        if (ACCEPT_IDENT("alias")) {
            EXPECT(Token::PAREN_OPEN);
            TRY(CROSS(tok<Token::STRINGCONST>)(ctx));
            EXPECT(Token::PAREN_CLOSE);
            ctx.stack.push(attribute_alias{});
        } else if (ACCEPT_IDENT("eraseable")) {
            ctx.stack.push(attribute_eraseable{});
        } else if (ACCEPT_IDENT("accumulate")) {
            ctx.stack.push(attribute_accumulate{});
        } else {
            ERR("unknown attribute '" + STRING() + "'");
        }
        EXPECT(Token::ATTRIBUTE_CLOSE);
        OK();
    }

    struct storageClass_typedef {
    };

    /// : 'typedef'
    /// | 'extern'
    /// | 'static'
    /// | 'noref'
    /// | 'local'  # legacy
    /// | 'var'    # legacy
    RULE(storageClassSpecifier) {
        static auto handleTypedef = [](ctx_t &ctx) {
            ctx.stack.push(storageClass_typedef{});
        };
        TRY(alt<
                when<Typedef, decltype(handleTypedef)>,
                Extern,
                Static,
                Noref,
                Local,
                Var
        >(ctx));
        OK();
    }

    /// : ('.' | '...')* directTypeSpecifier ('(' parameterTypeList ')')?
    RULE(typeSpecifier) {
        TRY(STAR(alt<tok<Token::DOT>, tok<Token::DOTS>>)(ctx));
        TRY(directTypeSpecifier(ctx));
        TRY(OPT(seq<tok<Token::PAREN_OPEN>, opt<parameterTypeList>, tok<Token::PAREN_CLOSE>>)(ctx));
        OK();
    }

    /// : ('void' | 'char' | 'int' | 'float' | 'vector' | 'string' | 'entity')
    /// | typedefName
    RULE(directTypeSpecifier) {
        TRY(alt<
                alt<
                        Void,
                        Char,
                        Int,
                        Float,
                        Vector,
                        String,
                        Entity
                >,
                typedefName
        >(ctx));
        OK();
    }

    /// : Identifier
    RULE(typedefName) {
        if (ACCEPT(Token::TYPENAME)) {
            OK();
        }
        if (PEEK() == Token::IDENT) {
            auto typedefs = ctx.typedefs;
            auto td = typedefs.find(std::string(STRING()));
            if (td != typedefs.end()) {
                ctx.next();
                OK();
            }
        }
        BT();
    }

    /// : 'const'
    RULE(typeQualifier) {
        TRY(Const(ctx));
        OK();
    }

    /// : Identifier (
    ///     '[' assignmentExpression_15? ']'
    ///   | '(' parameterTypeList? ')'
    /// )*
    RULE(declarator) {
        TRY(tok<Token::IDENT>(ctx));
        TRY(star<alt<
                seq<tok<Token::BRACKET_OPEN>, opt<assignmentExpression_15>, tok<Token::BRACKET_CLOSE>>,
                seq<tok<Token::PAREN_OPEN>, opt<parameterTypeList>, tok<Token::PAREN_CLOSE>>
        >>(ctx));
        OK();
    }

    /// : (
    ///     '[' assignmentExpression_15? ']'
    ///   | '(' parameterTypeList? ')'
    /// )+
    RULE(abstractDeclarator) {
        TRY(cross<alt<
                seq<tok<Token::BRACKET_OPEN>, opt<assignmentExpression_15>, tok<Token::BRACKET_CLOSE>>,
                seq<tok<Token::PAREN_OPEN>, opt<parameterTypeList>, tok<Token::PAREN_CLOSE>>
        >>(ctx));
        OK();
    }

    /// : (parameterVarargDeclaration | parameterDeclaration) (',' (parameterVarargDeclaration | parameterDeclaration))*
    RULE(parameterTypeList) {
        TRY(sep<alt<parameterVarargDeclaration, parameterDeclaration>, tok<Token::COMMA>>(ctx));
        OK();
    }

    /// : declarationSpecifiers (declarator | abstractDeclarator?)
    RULE(parameterDeclaration) {
        TRY(declarationSpecifiers(ctx));
        TRY(alt<
                declarator,
                opt<abstractDeclarator>
        >(ctx));
        OK();
    }

    /// : declarationSpecifiers? '...' Identifier?
    RULE(parameterVarargDeclaration) {
        TRY(OPT(declarationSpecifiers)(ctx));
        TRY(tok<Token::DOTS>(ctx));
        TRY(OPT(tok<Token::IDENT>)(ctx));
        OK();
    }

    /// : declarationSpecifiers initDeclaratorList? ';'
    RULE(declaration) {
        TRY(declarationSpecifiers(ctx));
        TRY(OPT(initDeclaratorList)(ctx));
        TRY(tok<Token::SEMICOLON>(ctx));

        // we're done, might have to register a typedef
//        auto typedefs = ctx.typedefs;
//        auto td = typedefs.find(std::string(STRING()));
//        if (td != typedefs.end()) {
//            ctx.next();
//            OK();
//        }
        OK();
    }

    /// : initDeclarator (',' initDeclarator)*
    RULE(initDeclaratorList) {
        TRY(sep<initDeclarator, tok<Token::COMMA>>(ctx));
        OK();
    }

    /// : declarator ('=' initializer)?
    RULE(initDeclarator) {
        TRY(seq<declarator, opt<seq<tok<Token::EQ>, initializer>>>(ctx));
        OK();
    }

    /// : assignmentExpression_15
    /// | '{' initializerList ','? '}'
    RULE(initializer) {
        TRY(alt<
                assignmentExpression_15,
                seq<tok<Token::BRACE_OPEN>, initializerList, opt<tok<Token::COMMA>>, tok<Token::BRACE_CLOSE>>
        >(ctx));
        OK();
    }

    /// : initializer (',' initializer)*
    RULE(initializerList) {
        TRY(sep<initializer, tok<Token::COMMA>>(ctx));
        OK();
    }

    /// : 'enum' '{' enumeratorList ','? '}'
    RULE(enumDeclaration) {
        TRY(Enum(ctx));
        TRY(tok<Token::BRACE_OPEN>(ctx));
        TRY(enumeratorList(ctx));
        TRY(OPT(tok<Token::COMMA>)(ctx));
        TRY(tok<Token::BRACE_CLOSE>(ctx));
        OK();
    }

    /// : enumerator (',' enumerator)*
    RULE(enumeratorList) {
        TRY(sep<enumerator, tok<Token::COMMA>>(ctx));
        OK();
    }

    /// : enumerationConstant ('=' constantExpression)?
    RULE(enumerator) {
        TRY(enumerationConstant(ctx));
        TRY(OPT(seq<tok<Token::EQ>, constantExpression>)(ctx));
        OK();
    }

    /// : Identifier
    RULE(enumerationConstant) {
        TRY(tok<Token::IDENT>(ctx));
        OK();
    }

    // statements

    /// : labeledStatement
    /// | compoundStatement
    /// | expressionStatement
    /// | selectionStatement
    /// | iterationStatement
    /// | jumpStatement
    RULE(statement) {
        TRY(alt<
                labeledStatement,
                compoundStatement,
                expressionStatement,
                selectionStatement,
                iterationStatement,
                jumpStatement
        >(ctx));
        OK();
    }

    /// : (
    ///     'case' constantExpression
    ///   | 'default'
    ///   |  Identifier
    ///   ) ':' statement
    ///   | ':' Identifier  # legacy
    RULE(labeledStatement) {
        constexpr auto legacy = seq<tok<Token::COLON>, tok<Token::IDENT>>;
        TRY(alt<
                seq<alt<
                        seq<Case, constantExpression>,
                        Default,
                        tok<Token::IDENT>
                >, tok<Token::COLON>, alt<statement, declaration>>, // declarations are an extension
                legacy
        >(ctx));
        OK();
    }

    /// : '{' blockItem* '}'
    RULE(compoundStatement) {
        TRY(tok<Token::BRACE_OPEN>(ctx));
        TRY(STAR(blockItem)(ctx));
        TRY(tok<Token::BRACE_CLOSE>(ctx));
        OK();
    }

    /// : declaration
    /// | statement
    RULE(blockItem) {
        TRY(alt<declaration, statement>(ctx));
        OK();
    }

    /// : expression? ';'
    RULE(expressionStatement) {
        TRY(OPT(expression)(ctx));
        TRY(tok<Token::SEMICOLON>(ctx));
        OK();
    }

    /// : 'if' '(' expression ')' statement ('else' statement)?
    /// | 'switch' '(' expression ')' statement
    RULE(selectionStatement) {
        TRY(alt<
                seq<If, tok<Token::PAREN_OPEN>, expression, tok<Token::PAREN_CLOSE>, statement, opt<seq<Else, statement>>>,
                seq<Switch, tok<Token::PAREN_OPEN>, expression, tok<Token::PAREN_CLOSE>, statement>
        >(ctx));
        OK();
    }

    /// : 'while' '(' expression ')' statement
    /// | 'do' statement 'while' '(' expression ')' ';'
    /// | 'for' '(' forCondition ')' statement
    RULE(iterationStatement) {
        TRY(alt<
                seq<While, tok<Token::PAREN_OPEN>, expression, tok<Token::PAREN_CLOSE>, statement>,
                seq<Do, statement, While, tok<Token::PAREN_OPEN>, expression, tok<Token::PAREN_CLOSE>, tok<Token::SEMICOLON>>,
                seq<For, tok<Token::PAREN_OPEN>, forCondition, tok<Token::PAREN_CLOSE>, statement>
        >(ctx));
        OK();
    }

    /// : (expression | forDeclaration)? ';' forExpression? ';' forExpression?
    RULE(forCondition) {
        /// : declarationSpecifiers initDeclaratorList?
        constexpr auto forDeclaration = seq<declarationSpecifiers, opt<initDeclaratorList>>;

        TRY(opt<alt<forDeclaration, expression>>(ctx));
        TRY(tok<Token::SEMICOLON>(ctx));
        TRY(opt<expression>(ctx));
        TRY(tok<Token::SEMICOLON>(ctx));
        TRY(opt<expression>(ctx));
        OK();
    }

    /// : 'goto' Identifier ';'
    /// | 'continue' ';'
    /// | 'break' ';'
    /// | 'return' expression? ';'
    RULE(jumpStatement) {
        TRY(alt<
                seq<Goto, tok<Token::IDENT>, tok<Token::SEMICOLON>>,
                seq<Continue, tok<Token::SEMICOLON>>,
                seq<Break, tok<Token::SEMICOLON>>,
                seq<Return, opt<expression>, tok<Token::SEMICOLON>>
        >(ctx));
        OK();
    }

    // expressions
    // left associative unless specified otherwise

    RULE(expression) {
        return commaExpression_16(ctx);
    }

    /// assignmentExpression_15 (',' assignmentExpression_15)*
    RULE(commaExpression_16) {
        TRY(leftop<
                assignmentExpression_15,
                tok<Token::COMMA>
        >(ctx));
        OK();
    }

    RULE(constantExpression) {
        return conditionalExpression(ctx);
    }

    /// : postfixExpression_2 assignmentOperator assignmentExpression_15
    /// | conditionalExpression
    /// right associative
    RULE(assignmentExpression_15) {
        constexpr auto assignmentOperator = alt<
                tok<Token::EQ>,
                seq<alt<
                        tok<Token::MUL>,
                        tok<Token::DIV>,
                        tok<Token::MOD>,
                        tok<Token::ADD>,
                        tok<Token::SUB>,
                        tok<Token::OP_LSH>,
                        tok<Token::OP_RSH>,
                        tok<Token::AND>,
                        tok<Token::XOR>,
                        tok<Token::OR>
                >, tok<Token::EQ>>
        >;
        TRY(alt<
                seq<alt<postfixExpression_2, Return>, assignmentOperator, assignmentExpression_15>,
                conditionalExpression
        >(ctx));
        OK();
    }

    /// : logicalOrExpression_14 ('?' expression ':' expression)?
    /// right associative
    RULE(conditionalExpression) {
        TRY(logicalOrExpression_14(ctx));
        TRY(OPT(seq<tok<Token::QUESTION>, expression, tok<Token::COLON>, expression>)(ctx));
        OK();
    }

    /// : logicalAndExpression_13 ('||' logicalAndExpression_13)*
    RULE(logicalOrExpression_14) {
        TRY(leftop<
                logicalAndExpression_13,
                seq<tok<Token::OR>, tok<Token::OR>>
        >(ctx));
        OK();
    }

    /// : inclusiveOrExpression_12 ('&&' inclusiveOrExpression_12)*
    RULE(logicalAndExpression_13) {
        TRY(leftop<
                inclusiveOrExpression_12,
                tok<Token::OP_AND>
        >(ctx));
        OK();
    }

    /// : exclusiveOrExpression_11 ('|' exclusiveOrExpression_11)*
    RULE(inclusiveOrExpression_12) {
        TRY(leftop<
                exclusiveOrExpression_11,
                tok<Token::OR>
        >(ctx));
        OK();
    }

    /// : andExpression_10 ('^' andExpression_10)*
    RULE(exclusiveOrExpression_11) {
        TRY(leftop<
                andExpression_10,
                tok<Token::XOR>
        >(ctx));
        OK();
    }

    /// : equalityExpression_9 ('&' equalityExpression_9)*
    RULE(andExpression_10) {
        TRY(leftop<
                equalityExpression_9,
                tok<Token::AND>
        >(ctx));
        OK();
    }

    /// : relationalExpression_8 (('==' | '!=') relationalExpression_8)*
    RULE(equalityExpression_9) {
        TRY(leftop<
                relationalExpression_8,
                seq<alt<
                        tok<Token::EQ>,
                        tok<Token::NOT>
                >, tok<Token::EQ>>
        >(ctx));
        OK();
    }

    /// : shiftExpression_7 (('<' | '<=' | '>' | '>=') shiftExpression_7)*
    RULE(relationalExpression_8) {
        TRY(leftop<
                shiftExpression_7,
                alt<
                        tok<Token::LT>,
                        tok<Token::OP_LE>,
                        tok<Token::GT>,
                        tok<Token::OP_GE>
                >
        >(ctx));
        OK();
    }

    /// : additiveExpression_6 (('<<' | '>>') additiveExpression_6)*
    RULE(shiftExpression_7) {
        TRY(leftop<
                additiveExpression_6,
                alt<
                        tok<Token::OP_LSH>,
                        tok<Token::OP_RSH>
                >
        >(ctx));
        OK();
    }

    /// : multiplicativeExpression_5 (('+' | '-') multiplicativeExpression_5)*
    RULE(additiveExpression_6) {
        TRY(leftop<
                multiplicativeExpression_5,
                alt<
                        tok<Token::ADD>,
                        tok<Token::SUB>
                >
        >(ctx));
        OK();
    }

    /// : castExpression_3 (('*' | '/' | '%' | '><') castExpression_3)*
    RULE(multiplicativeExpression_5) {
        TRY(leftop<
                castExpression_3,
                alt<
                        tok<Token::MUL>,
                        tok<Token::DIV>,
                        tok<Token::MOD>,
                        tok<Token::OP_CROSS>
                >
        >(ctx));
        OK();
    }

    /// : unaryExpression_3
    /// | '(' typeName ')' castExpression_3
    /// right associative
    RULE(castExpression_3) {
        // no casts yet
        return unaryExpression_3(ctx);
    }

    /// : postfixExpression_2 ('**' unaryExpression_3)*
    /// | ('++' | '--') unaryExpression_3
    /// | ('+' | '-' | '~' | '!') castExpression_3
    /// right associative
    RULE(unaryExpression_3) {
        TRY(alt<
                seq<postfixExpression_2, star<seq<tok<Token::MUL>, tok<Token::MUL>, unaryExpression_3>>>,
                seq<alt<
                        seq<tok<Token::ADD>, tok<Token::ADD>>,
                        seq<tok<Token::SUB>, tok<Token::SUB>>
                >, unaryExpression_3>,
                seq<alt<
                        tok<Token::ADD>,
                        tok<Token::SUB>,
                        tok<Token::BITNOT>,
                        tok<Token::NOT>
                >, castExpression_3>
        >(ctx));
        OK();
    }

    /// : primaryExpression (
    /// |   '[' expression ']'
    /// |   '(' expression? ')'
    /// |   '.' Identifier          # static field
    /// |   '.' '(' expression ')'  # computed field
    /// |   ('++' | '--')
    /// | )*
    RULE(postfixExpression_2) {
        TRY(primaryExpression(ctx));
        TRY(star<alt<
                seq<tok<Token::BRACKET_OPEN>, expression, tok<Token::BRACKET_CLOSE>>,
                seq<tok<Token::PAREN_OPEN>, opt<expression>, tok<Token::PAREN_CLOSE>>,
                seq<tok<Token::DOT>, tok<Token::IDENT>>,
                seq<tok<Token::DOT>, tok<Token::PAREN_OPEN>, expression, tok<Token::PAREN_CLOSE>>,
                seq<alt<
                        seq<tok<Token::ADD>, tok<Token::ADD>>,
                        seq<tok<Token::SUB>, tok<Token::SUB>>
                >>
        >>(ctx));
        OK();
    }

    /// : Identifier
    /// | Constant
    /// | StringLiteral+
    /// | '...' '(' assignmentExpression_15 ',' typeSpecifier ')'  # absolute va_arg
    /// | '(' expression ')'
    RULE(primaryExpression) {
        TRY(alt<
                tok<Token::IDENT>,
                tok<Token::INTCONST>,
                tok<Token::FLOATCONST>,
                tok<Token::CHARCONST>,
                tok<Token::VECTORCONST>,
                cross<tok<Token::STRINGCONST>>,
                seq<tok<Token::HASH>, tok<Token::INTCONST>>,
                seq<tok<Token::DOTS>, tok<Token::PAREN_OPEN>, assignmentExpression_15, tok<Token::COMMA>, typeSpecifier, tok<Token::PAREN_CLOSE>>,
                seq<tok<Token::PAREN_OPEN>, expression, tok<Token::PAREN_CLOSE>>
        >(ctx));
        OK();
    }
};

static bool parser_compile(ctx_t &&ctx) {
    ctx.parser.lex->flags.noops = true; // don't parse operators
    ctx.next();
    auto result = grammar::compilationUnit(ctx);
    if (result) {
        return true;
    }
    ctx.error(result.error.c_str());
    return false;
}

// utils

const int PARSER_HT_SIZE = 512;
const int TYPEDEF_HT_SIZE = 512;

parser_t *parser_create() {
    auto parser = new parser_t;
    for (size_t i = 0; i < operator_count; ++i) {
        if (operators[i].id == opid1('=')) {
            parser->assign_op = &operators[i];
            break;
        }
    }
    if (!parser->assign_op) {
        con_err("internal error: initializing parser: failed to find assign operator\n");
        delete parser;
        return nullptr;
    }

    return parser;
}

parser_t::parser_t()
        : lex(nullptr), tok(Token::NONE), ast_cleaned(false), translated(0), crc_globals(0), crc_fields(0),
          function(nullptr),
          aliases(util_htnew(PARSER_HT_SIZE)), htfields(util_htnew(PARSER_HT_SIZE)),
          htglobals(util_htnew(PARSER_HT_SIZE)), assign_op(nullptr), noref(false), max_param_count(1), m_fold(*this),
          m_intrin(*this) {
    variables.push_back(htfields);
    variables.push_back(htglobals);
    typedefs.push_back(util_htnew(TYPEDEF_HT_SIZE));
    _blocktypedefs.push_back(0);

    lex_ctx_t empty_ctx;
    empty_ctx.file = "<internal>";
    empty_ctx.line = 0;
    empty_ctx.column = 0;
    nil = new ast_value(empty_ctx, "nil", TYPE_NIL);
    nil->m_cvq = CV_CONST;
    if (OPTS_FLAG(UNTYPED_NIL))
        util_htset(htglobals, "nil", (void *) nil);

    const_vec[0] = new ast_value(empty_ctx, "<vector.x>", TYPE_NOEXPR);
    const_vec[1] = new ast_value(empty_ctx, "<vector.y>", TYPE_NOEXPR);
    const_vec[2] = new ast_value(empty_ctx, "<vector.z>", TYPE_NOEXPR);

    if (OPTS_OPTION_BOOL(OPTION_ADD_INFO)) {
        reserved_version = new ast_value(empty_ctx, "reserved:version", TYPE_STRING);
        reserved_version->m_cvq = CV_CONST;
        reserved_version->m_hasvalue = true;
        reserved_version->m_flags |= AST_FLAG_INCLUDE_DEF;
        reserved_version->m_flags |= AST_FLAG_NOREF;
        reserved_version->m_constval.vstring = util_strdup(GMQCC_FULL_VERSION_STRING);
    } else {
        reserved_version = nullptr;
    }
}

parser_t::~parser_t() {
    remove_ast();
}

void parser_t::remove_ast() {

}

bool parser_compile_string(parser_t &parser, const char *name, const char *str, size_t len) {
    parser.lex = lex_open_string(str, len, name);
    if (!parser.lex) {
        con_err("failed to create lexer for string \"%s\"\n", name);
        return false;
    }
    return parser_compile(ctx_t(parser));
}

bool parser_compile_file(parser_t &parser, const char *filename) {
    parser.lex = lex_open(filename);
    if (!parser.lex) {
        con_err("failed to open file \"%s\"\n", filename);
        return false;
    }
    return parser_compile(ctx_t(parser));
}

ast_expression *parser_find_global(parser_t &parser, const char *name) {
    auto ctx = ctx_t(parser);
    ast_expression *var = (ast_expression *) util_htget(parser.aliases, STRING());
    if (var)
        return var;
    return (ast_expression *) util_htget(parser.htglobals, name);
}

bool parser_finish(parser_t &parser, const char *output) {
    return true;
}
