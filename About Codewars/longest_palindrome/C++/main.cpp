int longest_palindrome(const std::string& s)
{
	const int len = s.length();
	for (auto count = len; count > 0; --count)
	{
		for (auto pos = 0; pos < len && len - pos >= count; ++pos)
		{
			auto ss = s.substr(pos, count);
			if (std::equal(ss.begin(), ss.end(), ss.rbegin()))
			{
				return count;
			}
		}
	}
    return 0;
}
