auto sumTwoSmallestNumbers = [](std::vector<int> numbers)
{
	std::sort(numbers.begin(), numbers.end());
	return (long)numbers[0] + numbers[1];
};
