-- Izomorfizmi

theorem eq1 {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  by
    apply Iff.intro
    . intro h
      apply And.intro
      . exact h.right
      . exact h.left
    . intro h
      apply And.intro
      . exact h.right
      . exact h.left

theorem eq2 {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
  by
    apply Iff.intro
    . intro h
      cases h
      . apply Or.inr
        assumption
      . apply Or.inl
        assumption
    . intro h
      cases h
      . apply Or.inr
        assumption
      . apply Or.inl
        assumption


theorem eq3 {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) :=
  by
    apply Iff.intro
    . intro h
      intros h.left h.left h.right
    . intro h
      sorry

theorem eq4 {A B C : Prop} : (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C)) :=
 sorry

theorem eq5 {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) :=
  sorry

theorem eq6 {A B C : Prop} : (B ∨ C) → A ↔ (B → A) ∧ (C → A) :=
  by
    apply Iff.intro
    . intro h
      constructor
      . intro hb
        apply h
        left
        assumption
      . intro hc
        apply h
        right
        assumption
    . intro h hbc
      cases hbc
      . apply h.left
        assumption
      . apply h.right
        assumption

theorem eq7 {A B C : Prop} : C → (A ∧ B) ↔ (C → A) ∧ (C → B) :=
  sorry
