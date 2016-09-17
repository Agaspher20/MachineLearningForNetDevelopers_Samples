using System;
using System.Diagnostics;

namespace CSharp
{
    public static class TestRun
    {
        public static void Run()
        {
            var trainingPath = @"C:\Users\Михаил\Documents\"
                            + @"Visual Studio 2015\Projects\"
                            + @"DigitsRecognizer\data\trainingsample.csv";
            Trace.WriteLine("Reading training set");
            var training = DataReader.ReadObservations(trainingPath);
            Trace.WriteLine($"Training set read. We have {training.Length} images.");

            Trace.WriteLine("Training finished.");
            var validationPath = @"C:\Users\Михаил\Documents\"
                            + @"Visual Studio 2015\Projects\"
                            + @"DigitsRecognizer\data\validationsample.csv";
            Trace.WriteLine("Reading validation set");
            var validation = DataReader.ReadObservations(validationPath);
            Trace.WriteLine($"Validation set read. We have {validation.Length} images.");
            
            ValidateDistanceFunction(
                training,
                validation,
                new ManhattanDistance(),
                "Manhattan");
            ValidateDistanceFunction(training,
                validation,
                new EuclideanDistance(),
                "Euclidean");
        }

        private static void ValidateDistanceFunction(
            Observation[] trainingSet,
            Observation[] validationSet,
            IDistance distance,
            string distanceName)
        {
            Trace.WriteLine("Classifier training started");
            var classifier = new BasicClassifier(distance);
            classifier.Train(trainingSet);
            Trace.WriteLine("Classifier training finished");
            Trace.WriteLine($"{distanceName} distance validation started");
            var stopWatch = new Stopwatch();
            stopWatch.Start();
            var correct = Evaluator.Correct(validationSet, classifier);
            stopWatch.Stop();
            Trace.WriteLine($"Classifier with {distanceName}"
                + $"\n\tCorrectly solved: {correct:P2}"
                + $"\n\tElapsed time: {stopWatch.Elapsed}");
        }
    }
}
