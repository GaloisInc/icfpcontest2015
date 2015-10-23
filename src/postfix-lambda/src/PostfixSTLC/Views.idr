module PostfixSTLC.Views

||| A view over Lists and Nats, that either extracts the element at
||| that index or explains why that can't be done.
data Nth : Nat -> List a -> Type where
  OutOfBounds : (LTE (List.length xs) n) -> Nth n xs
  Here : (xs : List a) -> (x : a) -> (ys : List a) -> (length xs = n) -> Nth n (xs ++ [x] ++ ys)

||| A view over Lists and Nats, that either extracts the element at
||| that index or explains why that can't be done.
nth : (n : Nat) -> (xs : List a) -> Nth n xs
nth Z [] = OutOfBounds LTEZero
nth Z (x :: xs) = Here [] x xs Refl
nth (S k) [] = OutOfBounds LTEZero
nth (S k) (x :: xs) with (nth k xs)
  nth (S k) (x :: xs)                | (OutOfBounds lte) = OutOfBounds (LTESucc lte)
  nth (S k) (x :: (ys ++ (y :: zs))) | (Here ys y zs prf) = Here (x :: ys) y zs (cong prf)

