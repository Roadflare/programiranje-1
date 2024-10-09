
theorem fu {A B C : Prop} : (A ∧ B → C ) → (A → B → C) :=
  by
    intros h ha hb
    --intro h
    --intro ha
    --intro hb
    apply h
    constructor
    -- apply And.intro
    exact ha
    exact hb




--Tactics:
--  rfl - reflexive ie: rfl 1 = 1
--  apply - if X -> C; enough to prove X
--  constructor And.intro - fun A → B -> A ∧ B
--  exact - apply with not propositions
--  constructor - constructs needed ∧ ∨
--
--
