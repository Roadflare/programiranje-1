-- Vzamemo stvari iz predavanj
set_option autoImplicit false

inductive Naravno : Type where
  | nic : Naravno
  | naslednik : Naravno → Naravno
deriving Repr


def plus : Naravno → Naravno → Naravno :=
  fun m n =>
    match m with
    | Naravno.nic => n
    | Naravno.naslednik m' =>
        Naravno.naslednik (plus m' n)

-- Vektorji

inductive Vektor : Type → Naravno → Type where
  | prazen : {A : Type} → Vektor A Naravno.nic
  | sestavljen : {A : Type} → {n : Naravno} → A → Vektor A n → Vektor A (Naravno.naslednik n)
deriving Repr

#check (Vektor.sestavljen "a" (Vektor.sestavljen "b" (Vektor.prazen)))

def stakni_vektorja : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
    match xs with
    | Vektor.prazen => ys
    | Vektor.sestavljen x xs' => Vektor.sestavljen x (stakni_vektorja xs' ys)


-- Sedaj lahko definiramo `lookup`, ki ne bo nikoli povzročil napake.
inductive Finite : Naravno -> Type where
  | fzero : {n : Naravno} -> Finite (Naravno.naslednik n)
  | fsucc : {n : Naravno} -> Finite n -> Finite (Naravno.naslednik n)


def downcast: a < b -> Finite a -> Finite b

def lookup {A : Type} {n : Naravno} : Vektor A n -> Finite n -> A :=
  fun xs i =>
    match xs, i with
    | Vektor.sestavljen x xs', Finite.fzero => x
    | Vektor.sestavljen _ xs', Finite.fsucc i' => lookup xs' i'


-- Včasih enakost tipov ni takoj očitna in jo moramo izpeljati
-- Dopolnite naslednjo definicijo, vse potrebne leme pa dokažite kar s taktiko `sorry`.

def plus_zero (n : Naravno) : (plus n Naravno.nic) = n := by
  sorry

def plus_add_suc (m n : Naravno) : (plus m (Naravno.naslednik n)) = (Naravno.naslednik (plus m n)) := by
  sorry

def plus_comm (m n : Naravno) : (plus m n) = (plus n m) := by
  sorry

-- xs ys
-- xs @ ys : Vector A (n + m)
-- xs @ ys : Vector A (m + n)
def stakni_vektorja' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus n m) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
    match xs with
    | Vektor.prazen => by
      have plus_nic : plus n Naravno.nic = n := sorry
      rw [plus_nic]
      exact ys
    | Vektor.sestavljen x xs' => by
      have v := Vektor.sestavljen x (stakni_vektorja' xs' ys)
      have add_succ {m n : Naravno} : plus m (Naravno.naslednik n) =
      Naravno.naslednik (plus m n) := sorry
      have add_comm : plus m n = plus n m := sorry
      rw [add_succ]
      exact v


-- Uporabite samo definicijo `stakni_vektorja'` in taktike `rw` in `exact`.
def stakni_vektorja'' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) => by
    have v := stakni_vektorja' xs ys
    have add_comm {m n : Naravno} : plus m n = plus n m := sorry
    rw [add_comm]
    exact v
