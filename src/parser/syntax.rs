//! Definitions for various syntactic constructs.
//!
//! Taken from: https://docs.python.org/3/library/ast.html#abstract-grammar

use crate::lexer::Op;

pub type Identifer<'a> = &'a str;

enum ExprNode {}

enum AnnotationNode {}

struct Alias<'src> {
    name: Identifer<'src>,
    asname: Identifer<'src>,
}

enum StatmentNode<'src> {
    /// Bare assignment statement, single or multi target.
    Assign {
        targets: Vec<ExprNode>,
        values: ExprNode,
    },
    /// Augmented assignment statment that combines a binary op & assignment.
    AugAssign {
        target: ExprNode,
        op: Op,
        value: ExprNode,
    },
    /// Assignment statment with annotations
    AnnAssign {
        target: ExprNode,
        annotation: AnnotationNode,
    },
    /// `del expr`
    Delete {
        targets: Vec<ExprNode>,
    },

    Import {
        names: Vec<Alias<'src>>,
    },
    ImportFrom {
        module: Option<Identifer<'src>>,
    },

    Return {
        value: Option<ExprNode>,
    },
    Pass,
    Break,
    Continue,
}

struct ModuleNode<'src> {
    body: Vec<StatmentNode<'src>>,
}

enum SyntaxNode<'src> {
    Module(ModuleNode<'src>),
}
