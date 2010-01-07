
-- | Basic types for algorythms on ESN
module Data.Neural.Signal where


-- | neuron names
type Key = Int 
-- | the signal component type
type Component = Double
-- | a signal hiding its implementation
type Signal = Key -> Component

-- | a pattern to be classified is a the set of components of the signal referred as inputs
type Pattern = [Component]

-- | evolution of a signal stopper
type Sword = [Signal] -> Signal

-- | the value of a calassification
type Class = Double

-- | a classification relation
type Fact = (Pattern,Class)




	
			
	
	
