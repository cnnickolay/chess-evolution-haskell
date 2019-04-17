module Chess.NeuralNetwork where

import System.Random

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + e ** (-x)) where e = exp 1.0

data Node = Node [Float] (Maybe Float) deriving Show
type Layer = [Node]
type NN = [Layer]

generateLayer :: Int -> Int -> Layer
generateLayer ws inp = (\_ -> Node (replicate ws 0) Nothing) <$> [1..inp]

-- | Generates Neural Network with given topology
network :: [Int] -> NN
network (l:ls) = reverse $ foldl addLayer [fl] ls
                      where fl = generateLayer 0 l
                            addLayer a@(fl:_) slt = (generateLayer (length fl) slt) : a

calculateNodeOutput :: Layer -> Node -> Node
calculateNodeOutput pl (Node ws _) =
  let
    weightsNodes = zip ws pl
    sum' = foldl (+) 0.0 $ fmap (\(w, (Node _ (Just out))) -> w * out) weightsNodes
  in
    Node ws $ Just (sigmoid sum')

fuseLayers :: Layer -> Layer -> Layer
fuseLayers pl l = fmap (calculateNodeOutput pl) l

initOutputs :: [Float] -> Layer -> Layer
initOutputs inp l = (\(Node k _, b) -> Node k (Just b)) <$> (zip l inp)

execute :: NN -> [Float] -> [Float]
execute (l:ls) inp =
  let
    li = initOutputs inp l
    r = foldl fuseLayers li ls
  in
    (\(Node _ (Just out)) -> out) <$> r

executeMultiple :: [NN] -> [Float] -> [(NN, [Float])]
executeMultiple nns inp = fmap (\nn -> (nn, execute nn inp)) nns

mutateWeight :: Float -> Float -> IO Float
mutateWeight m w = do
                    let r = w + m
                    rnd <- randomRIO (-m, m)
                    let mutated = rnd + w
                    return mutated

mutateNode :: Float -> Node -> IO Node
mutateNode m (Node w o) = do
                            mutatedWeights <- sequence $ fmap (mutateWeight m) w
                            return $ Node mutatedWeights o

mutateLayer :: Float -> Layer -> IO Layer
mutateLayer m l = sequence $ fmap (mutateNode m) l

mutateNetwork :: Float -> NN -> IO NN
mutateNetwork mutation nn = sequence $ fmap (mutateLayer mutation) nn

--selectFittest :: ([Float] -> Float) -> Float -> NN -> NN
--selectFittest fitness survivalRate nn =

breedNN :: Float -> Int -> NN -> IO [NN]
breedNN mutation amount nn = sequence $ replicate amount (mutateNetwork mutation nn)

squashNNs :: [NN] -> NN
squashNNs (nn:nns) = foldl squash nn nns where
                        squash nn1 nn2 = squashLayer <$> zip nn1 nn2
                        squashLayer (l1, l2) = squashNode <$> zip l1 l2
                        squashNode ((Node w1 _), (Node w2 _)) = Node ((\(w11, w22) -> (w11 + w22) / 2) <$> zip w1 w2) Nothing
