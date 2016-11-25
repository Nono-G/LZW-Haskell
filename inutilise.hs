--
-- TRUCS INUTILISES MAIS QUI POURRAIENT RESSERVIR UN JOUR PEUT ETRE

alLast :: AssoList -> (String,Code)
alLast (C paire Vide) = paire
alLast (C paire suite) = alLast suite


alV :: AssoList
alV = Vide
al1 :: AssoList
al1 = C ("a",0) (C ("b",1) (C ("ab",2) (C ("c",3) (C ("bc",4) Vide))))
--al2 :: AssoList
--al2 C () (C () (C () ()))

