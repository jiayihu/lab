f = 3;
g x y = let z = x in z;
h x = case (let y = x in y) of
  <1> -> 2;
  <2> -> 5;

-- comment
Leaf = Pack{1,1};
Branch = Pack{2,2};

f x y = case x of
  <1> -> case y of
    <1> -> 1;
  <2> -> 2
