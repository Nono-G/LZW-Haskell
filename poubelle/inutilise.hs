--
--
-- blabla
alLast :: AssoList -> (String,Code)
alLast (C paire Vide) = paire
alLast (C paire suite) = alLast suite
--al2 :: AssoList
--al2 C () (C () (C () ()))
