%------------------------------------------------------------------------------
% File     : SYN000-2-TCF : Modified from SYN000-1 for TCF
% Domain   : Syntactic
% Problem  : Advanced TPTP CNF syntax
% Version  : Biased.
% English  : 

% Refs     :
% Source   : [TPTP]
% Names    :


% Comments :
%------------------------------------------------------------------------------
%----Quoted symbols
tcf(distinct_object,axiom,
    ( "An Apple" != "A \"Microsoft \\ escape\"" )).

%----Roles - seen axiom already
tcf(role_definition,definition,
    f(d) = f(X) ).

tcf(role_assumption,assumption,
    p(a) ).

tcf(role_lemma,lemma,
    p(l) ).

tcf(role_theorem,theorem,
    p(t) ).

tcf(role_unknown,unknown,
    p(u) ).

%----Selective include directive
include('Axioms/SYN000-0.ax',[ia1,ia3]).

%----Source
tcf(source_unknown,axiom,
    p(X),
    unknown).

tcf(source,axiom,
    p(X),
    file('SYN000-1.p')).

tcf(source_name,axiom,
    p(X),
    file('SYN000-1.p',source_unknown)).

tcf(source_copy,axiom,
    p(X),
    source_unknown).

tcf(source_introduced_assumption,axiom,
    p(X),
    introduced(assumption,[from,the,world])).

tcf(source_inference,axiom,
    p(a),
    inference(magic,
        [status(thm),assumptions([source_introduced_assumption])],
        [theory(equality),source_unknown])  ).

tcf(source_inference_with_bind,axiom,
    p(a),
    inference(magic,
        [status(thm)],
        [theory(equality),source_unknown:[bind(X,$fot(a))]])  ).

%----Useful info
tcf(useful_info,axiom,
    p(X),
    unknown,
    [simple,
     prolog(like,Data,[nested,12.2]),
     AVariable,
     12.2,
     "A distinct object",
     $cnf(p(X) | ~q(X,a) | r(X,f(Y),g(X,f(Y),Z)) | ~ s(f(f(f(b))))),
     data(name):[colon,list,2],
     [simple,prolog(like,Data,[nested,12.2]),AVariable,12.2]
    ]).

%------------------------------------------------------------------------------
