namespace CSharp
{
    using System.Collections.Generic;
    using System.Linq;

    public sealed class Evaluator
    {
        public static double Correct(IEnumerable<Observation> validationSet, IClassifier classifier)
        {
            return validationSet
                .Select(obs => Score(obs, classifier))
                .Average();
        }

        private static double Score(Observation obs, IClassifier classifier)
        {
            var predictResult = classifier.Predict(obs.Pixels);
            return predictResult != null && predictResult == obs.Label
                ? 1.0
                : 0.0;
        }
    }
}
