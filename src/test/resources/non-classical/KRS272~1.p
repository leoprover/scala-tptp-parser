%------------------------------------------------------------------------------
% File     : KRS272~1 : TPTP v9.0.0. Released v9.0.0.
% Domain   : Natural language processing
% Problem  : Generation of abstract instructions
% Version  : Especial.
% English  : 

% Refs     : [Sto00] Stone (2000), Towards a Computational Account of Knowl
% Source   : [QMLTP]
% Names    : APM003+1 [QMLTP]

% Status   : CounterSatisfiable
%          : cumulative_T : CounterSatisfiable
%          : cumulative_S4 : Theorem
%          : varying_S4 : CounterSatisfiable
%          : constant_S5 : Theorem
% Rating   : ? v9.0.0
% Syntax   : TBA
% SPC      : TFN_THM_NEQ

% Comments : Slightly adapted from the original example: used only one modal 
%            operator for both user and common-ground beliefs.
%------------------------------------------------------------------------------
%----All four of these semantic specifications could be put in a file and 
%----included here. Or ... (see next comment) ...
tff(cumulative_T,logic,(
    $modal ==
        [ $logicfile == 'LOG002_1.l',
          $constants == $rigid,
          $quantification == $cumulative,
          $consequence == $global,
          $modalities == $modal_system_T ] )).

%----The semantic specification above might be the primary one and left in
%----this problem file, with the next three put in an include file.
tff(varying_S4,logic,(
    $modal ==
        [ $logicfile == 'LOG002_1.l',
          $constants == $rigid,
          $quantification == $varying,
          $consequence == $global,
          $modalities == $modal_system_S4 ] )).

tff(cumulative_S4,logic,(
    $modal ==
        [ $logicfile == 'LOG002_1.l',
          $constants == $rigid,
          $quantification == $cumulative,
          $consequence == $global,
          $modalities == $modal_system_S4 ] )).

tff(constant_S5,logic,(
    $modal ==
        [ $logicfile == 'LOG002_1.l',
          $constants == $rigid,
          $quantification == $constant,
          $consequence == $global,
          $modalities == $modal_system_S5 ] )).

tff(u_type,type,u: $i ).

tff(one_type,type,one: $i ).

tff(userid_type,type,userid: ($i * $i) > $o ).

tff(string_type,type,string: $i > $o ).

tff(entry_box_type,type,entry_box: $i > $o ).

tff(number_type,type,number: ($i * $i) > $o ).

tff(do_type,type,do: ($i * $i * $i) > $o ).

tff(in_type,type,in: ($i * $i * $i) > $o ).

tff(ax1,axiom,
    [.](
      ? [I: $i] : 
        [.]
          ( userid(u,I) 
          & string(I) ) ) ).

tff(ax2,axiom,
    ? [B: $i] : 
      [.]
        ( entry_box(B) 
        & number(B,one) ) ).

tff(ax3,axiom,
    [.](
      ! [S: $i,I: $i,B: $i] : 
        ( (string(I) 
          & entry_box(B) ) 
       => ? [A: $i] : 
            [.](
              ! [S2: $i] : 
                ( do(S,A,S2) 
               => in(I,B,S2) ) ) ) ) ).

tff(con,conjecture,
    [.](
      ? [I: $i,B: $i,A: $i,S: $i] : 
        ( [.]
            ( userid(u,I) 
            & entry_box(B) 
            & number(B,one) )
        & [.](
            ! [S2: $i] : 
              ( do(S,A,S2) 
             => in(I,B,S2) ) ) ) ) ).

%------------------------------------------------------------------------------
