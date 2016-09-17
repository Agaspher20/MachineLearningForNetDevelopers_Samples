namespace CSharp
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public sealed class BasicClassifier : IClassifier
    {
        private IEnumerable<Observation> _data;

        private readonly IDistance distance;

        public BasicClassifier(IDistance distance)
        {
            this.distance = distance;
        }

        public void Train(IEnumerable<Observation> trainingSet)
        {
            _data = trainingSet
                .Where(ts => !string.IsNullOrWhiteSpace(ts.Label) && ts.Pixels.Length > 0);
        }

        public string Predict(int[] pixels)
        {
            Observation currentBest = null;
            var shortest = double.MaxValue;

            foreach (Observation obs in _data)
            {
                try
                {
                    var dist = this.distance.Between(obs.Pixels, pixels);
                    if (dist < shortest)
                    {
                        shortest = dist;
                        currentBest = obs;
                    }
                }
                catch (ArgumentException)
                {
                    continue;
                }
            }

            return currentBest?.Label;
        }
    }
}
