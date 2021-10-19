%------------------------------------------------------------------------------
% File     : SYN000-1-TCF : Modified from SYN000-1 for TCF
% Domain   : Syntactic
% Problem  : Basic TPTP CNF syntax
% Version  : Biased.
% English  : Basic TPTP CNF syntax that you can't survive without parsing.

% Refs     :
% Source   : [TPTP]
% Names    :



% Comments :
%------------------------------------------------------------------------------
%----Propositional
tcf(propositional,axiom,
    ( p0
    | ~ q0
    | r0
    | ~ s0 )).

%----First-order
tcf(first_order,axiom,
    ( p(X)
    | ~ q(X,a)
    | r(X,f(Y),g(X,f(Y),Z))
    | ~ s(f(f(f(b)))) )).

tcf(first_order_tcf,axiom, ! [X:$i]:
    ( p(X)
    | ~ q(X,a)
    | r(X,f(Y),g(X,f(Y),Z))
    | ~ s(f(f(f(b)))) )).

tcf(first_order_tcf2,axiom, ! [X,Y,Z]:
    ( p(X)
    | ~ q(X,a)
    | r(X,f(Y),g(X,f(Y),Z))
    | ~ s(f(f(f(b)))) )).

tcf(first_order_tcf3,axiom, ! [X:$i,Y,Z]:
    ( p(X)
    | ~ q(X,a)
    | r(X,f(Y),g(X,f(Y),Z))
    | ~ s(f(f(f(b)))) )).

%----Equality
tcf(equality,axiom,
    ( f(Y) = g(X,f(Y),Z)
    | f(f(f(b))) != a
    | X = f(Y) )).

%----True and false
tcf(true_false,axiom,
    ( $true
    | $false )).

%----Quoted symbols
tcf(single_quoted,axiom,
    ( 'A proposition'
    | 'A predicate'(Y)
    | p('A constant')
    | p('A function'(a))
    | p('A \'quoted \\ escape\'') )).

%----Connectives - seen them all already

%----Annotated formula names
tcf(123,axiom,
    ( p(X)
    | ~ q(X,a)
    | r(X,f(Y),g(X,f(Y),Z))
    | ~ s(f(f(f(b)))) )).

%----Roles - seen axiom already
tcf(role_hypothesis,hypothesis,
    p(h)).

tcf(role_negated_conjecture,negated_conjecture,
    ~ p(X)).

%----Include directive
include('Axioms/SYN000-0.ax').

%----Comments
/* This
   is a block
   comment.
*/

%------------------------------------------------------------------------------
