#include <algorithm>
#include <iterator>
#include <iostream>
#include <queue>
#include <utility>

struct Fuck
{
	std::string s;
	int d;
	int c;
	int sum;
	Fuck(std::string s, const int d, const int c) :s(std::move(s)), d(d), c(c), sum(d + c) {}
};

auto Cmp = [](const Fuck& l, const Fuck& r)
{
	if (l.sum == r.sum)
	{
		if (l.d == r.d)
		{
			return l.s > r.s;
		}
		return l.d < r.d;
	}
	return l.sum < r.sum;
};

using PQ = std::priority_queue<Fuck, std::vector<Fuck>, decltype(Cmp)>;

int main()
{
	int n, l, h;
	PQ q0(Cmp);
	PQ q1(Cmp);
	PQ q2(Cmp);
	PQ q3(Cmp);
	std::cin >> n >> l >> h;
	for (auto i = 0; i < n; ++i)
	{
#define Emplace(q) q.push(Fuck(s, d, c))
		std::string s;
		int d, c;
		std::cin >> s >> d >> c;
		if (d >= h && c >= h)
		{
			Emplace(q0);
		}
		else if (d >= h && c >= l)
		{
			Emplace(q1);
		}
		else if (d >= l && c >= l && d >= c)
		{
			Emplace(q2);
		}
		else if (d >= l && c >= l)
		{
			Emplace(q3);
		}
	}
	std::cout << q0.size() + q1.size() + q2.size() + q3.size() << "\n";
#define Print(q)\
		while (!(q).empty())\
		{\
			std::cout << (q).top().s << " " << (q).top().d << " " << (q).top().c << "\n";\
			(q).pop();\
		}
	Print(q0);
	Print(q1);
	Print(q2);
	Print(q3);
}
