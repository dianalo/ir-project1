README
-------

For each algorithm we have
- {Algorithm}.scala file containing the definitions of the necessary functions
- {Algorithm}Script.scala file that can be run and performs learning on a training set and classification of a test set

To reproduce the submitted results, for each algorithm do:

1) Edit path argument in 'computeProbabilities'/'computeParameters' function to point to training files.
2) Edit path argument in 'classify' function to point to test files.
(REMARK: the training files need to be zipped individually due to the ReutersRCVStream workings)
3) Use parameters given below for constructors, 'computeParameters', 'classify' functions

NaiveBayes: 		NaiveBayesLec(...) unchanged
					computeProbabilities(path)
					classify(path, 0.3)
			
LogisticRegression:	LogisticRegression(config, theta, 100000)
					computeParameters(path, 1000, 1.0/100000)
					classify(path, 0.5)
					
SVM:				SVM(config, theta, 1.0, 100000)
					computeParameters(path, 1000)
					classify(path)
					
4) compile with Maven
5) run