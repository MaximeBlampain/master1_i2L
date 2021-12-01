% CC1 : Mini-projet de modélisaiton en MiniZinc
% REMINDER : 
%    /\ -> &&
%    \/ -> ||
% vars du dataset

int: nbOfBucket;
array[1..nbOfBucket] of int: bucketsCapacity;
array[1..nbOfBucket] of int: initialState;
array[1..nbOfBucket] of int: finalState;
int: maxStep;

% vars du model
var 1..maxStep: goalActualStep;
array[1..maxStep,1..nbOfBucket] of var int: actualState ;

solve minimize goalActualStep;

% containtes
constraint
 
  % check si l'initialState & le finalState de chaque sceau n'est pas plus grand que bucketsCapacity
  forall(bucket in 1..nbOfBucket) (
    (initialState[bucket] <= bucketsCapacity[bucket]) /\ (finalState[bucket] <= bucketsCapacity[bucket])
  ) /\
  % check que pendant le traitement le conteny d'actualState ne soit pas inférieur à 0 ou supérieur à bucketsCapacity
  forall(step in 1..maxStep,bucket in 1..nbOfBucket) (
    actualState[step,bucket] >= 0 /\ actualState[step,bucket] <= bucketsCapacity[bucket]
  ) /\
  % check que la somme de toute l'eau de l'initialState et du finalState sont identiques
  sum(bucket in 1..nbOfBucket)(initialState[bucket]) = sum(bucket in 1..nbOfBucket)(finalState[bucket]) /\
  % check que l'état de départ et l'état d'arrivé correspondent à l'initialState et au finalState
  forall(bucket in 1..nbOfBucket) (
    actualState[1,bucket] = initialState[bucket] /\ actualState[goalActualStep,bucket] = finalState[bucket]
  ) /\
  
  forall(step in 1..maxStep-1) (
      % Le contenu de seulement 2 seaux changent
      sum(bucket in 1..nbOfBucket)( bool2int(actualState[step,bucket] != actualState[step+1, bucket])) = 2
      /\
      % La somme d'eau est la meme à tout moment de la simulation
      sum(bucket in 1..nbOfBucket)(actualState[step,bucket]) = sum(bucket in 1..nbOfBucket)(actualState[step+1,bucket])
      /\
 
      % Après chaque étape, soit le seau qu'on a transvidé devient vide, soit le seau cible devient plein
      forall(bucket1, bucket2 in 1..nbOfBucket where bucket1 != bucket2) (
         ((actualState[step, bucket1] != actualState[step+1, bucket1]) /\ (actualState[step, bucket2] != actualState[step+1, bucket2])) ->
            (actualState[step+1,bucket1] = 0 \/ actualState[step+1,bucket1] = bucketsCapacity[bucket1] \/
             actualState[step+1,bucket2] = 0 \/ actualState[step+1,bucket2] = bucketsCapacity[bucket2])
     )
   )
;
 
 
output [
  if bucket = 1 then "\n step " ++ show(step) ++ " : " else " " endif ++
  show(actualState[step, bucket])
  | step in 1..maxStep, bucket in 1..nbOfBucket
]
++ ["\n"];
  