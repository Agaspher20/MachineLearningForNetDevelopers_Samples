using System;

namespace CSharp
{
    public sealed class EuclideanDistance : IDistance
    {
        public double Between(int[] pixels1, int[] pixels2)
        {
            if (pixels1.Length != pixels2.Length)
            {
                throw new ArgumentException("Inconsistent image sizes.");
            }

            int stepsCount = pixels1.Length;
            double sum = 0;
            for(int i = 0; i < stepsCount; ++i)
            {
                double difference = pixels1[i] - pixels2[i];
                sum += difference * difference;
            }

            return sum;
        }
    }
}
