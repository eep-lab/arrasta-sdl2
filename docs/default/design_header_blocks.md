## EndSessionOnCriterion
Se `True` a sessão é finalizada após o desempenho atingir o critério do bloco.
Se `False`, desabilitado.

## EndSessionOnNotCriterionAfterBlockRepetitions
Se `True` a sessão é finalizada após o desempenho **não** atingir o critério do bloco.
Se `False`, desabilitado.

## RepeatStyle
Controla como um mesmo bloco é repetido caso o desempenho não atinja o critério do bloco.

Se `None`, desabilitado, nenhuma repetição é realizada.
Se `Consecutive`, o bloco é repetido `MaxBlockRepetitionConsecutives` vezes.
Se `Global`, o bloco é repetido `MaxBlockRepetitionInSession` vezes, porém não bloqueia o fluxo quando `NextBlockOnNotCriterion` > -1.
Se `ConsecutiveAndGlobal`, o bloco utiliza ambos `Consecutive` e `Global`.

## MaxBlockRepetitionConsecutives
Número de repetições permitidas no bloco. Ver `RepeatStyle`.

## MaxBlockRepetitionInSession
Número de repetições permitidas no bloco. Ver `RepeatStyle`.

## EndCriterionStyle
Se `HitCount`, o número de acertos é usado como critério de desempenho.
Se `MissCount`, o número de erros é usado como critério de desempenho.
Se `ConsecutiveHits`, o número de acertos consecutivos é usado como critério de desempenho.
Se `ConsecutiveMisses`, o número de erros consecutivos é usado como critério de desempenho.
Se `HitPorcentage`, a porcentagem de acertos é usada como critério de desempenho.
Se `MissPorcentage`, a porcentagem de erros é usada como critério de desempenho.

## EndCriterionEvaluationTime
Se `OnTrialEnd`, o critério de desempenho do bloco é avaliado tentativa à tentativa.
Se `OnBlocEnd`, o critério de desembenho do bloco é avaliado ao final da última tentativa do bloco.

## NextBlockOnCriterion
ID do bloco seguinte quando o critério de desempenho é atingido em um bloco.

## NextBlockOnNotCriterion
ID do bloco seguinte quando o critério de desempenho **não** é atingido em um bloco.

## EndCriterionValue
Valor do critério de desempenho. Ver `EndCriterionStyle`.

## Reinforcement
Porcentagem de tentativas do bloco com reforçamento diferencial.