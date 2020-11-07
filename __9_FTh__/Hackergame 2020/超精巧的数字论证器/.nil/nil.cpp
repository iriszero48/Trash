#include <set>
#include <iostream>
#include <vector>
#include <sstream>
#include <map>
#include <regex>
#include <functional>
#include <algorithm>
#include <execution>
#include <unordered_map>
#include <mutex>
#include <tuple>
#include <array>
#include <string_view>
#include <charconv>

void step1()
{
	std::set<std::tuple<int, int, int, int, int>> st{};
#define fuck(i)	for(int i = 0;i<2;++i)
	fuck(i)
		fuck(j)
		fuck(k)
		fuck(l)
		fuck(m)
		st.emplace(i, j, k, l, m);
	std::ostringstream ss{};
#undef fuck
#define fuck(d,s) << d << (s==1?",":"")
	for (const auto& [a, b, c, d, e] : st)
	{
		std::cout << "["
			fuck(1, a)
			fuck(1, b)
			fuck(4, c)
			fuck(5, d)
			fuck(1, e)
			fuck(4, 0)
			<< "],\n";
	}
}

//step1();
const std::vector<std::vector<int>> S1d
{
	{11451, 4},
	{1145, 14},
	{1145, 1, 4},
	{114, 514},
	{114, 51, 4},
	{114, 5, 14},
	{114, 5, 1, 4},
	{11, 4514},
	{11, 451, 4},
	{11, 45, 14},
	{11, 45, 1, 4},
	{11, 4, 514},
	{11, 4, 51, 4},
	{11, 4, 5, 14},
	{11, 4, 5, 1, 4},
	{1, 14514},
	{1, 1451, 4},
	{1, 145, 14},
	{1, 145, 1, 4},
	{1, 14, 514},
	{1, 14, 51, 4},
	{1, 14, 5, 14},
	{1, 14, 5, 1, 4},
	{1, 1, 4514},
	{1, 1, 451, 4},
	{1, 1, 45, 14},
	{1, 1, 45, 1, 4},
	{1, 1, 4, 514},
	{1, 1, 4, 51, 4},
	{1, 1, 4, 5, 14},
	{1, 1, 4, 5, 1, 4}
};

void step2()
{
	std::set<int> dt{};
	for (const auto& s1d : S1d)
	{
		for (auto value : s1d)
		{
			dt.emplace(value);
		}
	}
	for (auto value : dt)
	{
		//std::cout << "static const std::string_view _" << value << " = " << "\"" << value << "\";" << "\n";
		std::cout << "{" << value << ", " << "\"" << value << "\"}," << "\n";
	}
}

static int Nil(const int x) { return  x; }
static int Neg(const int x) { return -x; }
static int Not(const int x) { return ~x; }

static const std::map<const std::array<const std::string_view, 2>, const std::array<const std::function<int(int)>, 2>> Op1Comps
{
	{{ " ", " " }, { Nil, Nil }},
	{{ " ", "-" }, { Nil, Neg }},
	{{ " ", "~" }, { Nil, Not }},
	{{ "-", " " }, { Neg, Nil }},
	{{ "-", "-" }, { Neg, Neg }},
	{{ "-", "~" }, { Neg, Not }},
	{{ "~", " " }, { Not, Nil }},
	{{ "~", "-" }, { Not, Neg }},
	{{ "~", "~" }, { Not, Not }},
};
//std::set<std::tuple<std::pair<char, std::function<int(int)>>, std::pair<char, std::function<int(int)>>>> Op1Comps{};

static const std::unordered_map<std::string_view, std::function<int(int, int)>> Ops
{
	{ "+", [](const int a, const int b) { return a + b; } },
	{ "-", [](const int a, const int b) { return a - b; } },
	{ "*", [](const int a, const int b) { return a * b; } },
	{ "/", [](const int a, const int b) { const auto tmp = a / b; return tmp < 0 ? tmp - 1 : tmp; } },
	{ "%", [](const int a, const int b) { return (b + a % b) % b; } },
	{ "^", [](const int a, const int b) { return a ^ b; } },
	{ "&", [](const int a, const int b) { return a & b; } },
	{ "|", [](const int a, const int b) { return a | b; } },
};

static const std::unordered_map<int, std::string_view> NumStringPool
{
	{1, "1"},
	{4, "4"},
	{5, "5"},
	{11, "11"},
	{14, "14"},
	{45, "45"},
	{51, "51"},
	{114, "114"},
	{145, "145"},
	{451, "451"},
	{514, "514"},
	{1145, "1145"},
	{1451, "1451"},
	{4514, "4514"},
	{11451, "11451"},
	{14514, "14514"},
};

auto Re = std::regex(".*1.*1.*4.*5.*1.*4.*");

static bool Qed = false;

static std::mutex QedLock{};

void SetQed()
{
	std::lock_guard<decltype(QedLock)> lock(QedLock);
	Qed = true;
}

static const std::string_view LeftParenthesis = "(";
static const std::string_view RightParenthesis = ")";

static void Solve(const std::vector<int>& num, const std::vector<std::vector<std::string_view>>& how, int target);

#define um

static void SolveLoop(const std::vector<int>& num, const std::vector<std::vector<std::string_view>>& how, const int target,
	const decltype(num.size()) i, const std::pair<std::string_view, std::function<int(int, int)>>& kv)
{
	const auto j = i + 1;
	const auto& [k, v] = kv;
	for (const auto& [ks, vs] : Op1Comps)
	{
		if (Qed) continue;
		const auto& [n1k, n2k] = ks;
		const auto& [n1v, n2v] = vs;
		const auto n1 = n1v(num[i]);
		const auto n2 = n2v(num[j]);
		if (n2 == 0 && (k == "/" || k == "%")) continue;
		std::vector<int> newNum{};
#define cv(v) std::copy(v.begin(), v.end(), std::back_inserter(ss));
#define nk2s(nk, v) if (nk == " ") { cv(v); }else { ss.push_back(LeftParenthesis); ss.push_back(nk); cv(v); ss.push_back(RightParenthesis); }
		std::vector<std::string_view> ss{};
		ss.push_back(LeftParenthesis);

#ifdef um
		nk2s(n1k, how[i]);
#else
		if (n1k == " ")
		{
			std::copy(how[i].begin(), how[i].end(), std::back_inserter(ss));
		}
		else
		{
			ss.push_back(LeftParenthesis);
			ss.push_back(n1k);
			std::copy(how[i].begin(), how[i].end(), std::back_inserter(ss));
			ss.push_back(RightParenthesis);
		}
#endif
		
		ss.push_back(k);

#ifdef um
		nk2s(n2k, how[j]);
#else
		if (n2k == " ")
		{
			std::copy(how[j].begin(), how[j].end(), std::back_inserter(ss));
		}
		else
		{
			ss.push_back(LeftParenthesis);
			ss.push_back(n2k);
			std::copy(how[j].begin(), how[j].end(), std::back_inserter(ss));
			ss.push_back(RightParenthesis);
		}
#endif
		
		ss.push_back(RightParenthesis);
		std::vector<std::vector<std::string_view>> newHow{};
		for (decltype(num.size()) index = 0; index < num.size(); ++index)
		{
			if (index == i)
			{
				newNum.push_back(v(n1, n2));
				newHow.push_back(std::move(ss));
				++index;
			}
			else
			{
				newNum.push_back(num[index]);
				newHow.push_back(how[index]);
			}
		}

		Solve(newNum, newHow, target);
	}
}

static void Solve(const std::vector<int>& num, const std::vector<std::vector<std::string_view>>& how, const int target)
{
	if (Qed) return;
	if (num.size() == 1/* && std::regex_match(how[0], Re)*/)
	{
		//if (!std::regex_match(how[0], Re)) { throw std::runtime_error("???"); }
		auto done = false;
		std::function<std::stringstream()> func = [&]() { std::stringstream ss{}; std::copy(how[0].begin(), how[0].end(), std::ostream_iterator<std::string_view>(ss)); ss << "\n"; return ss; };
		if (num[0] == target) { done = true; }
		if (Neg(num[0]) == target) { done = true; func = [&]() { std::stringstream ss{}; ss << "-"; std::copy(how[0].begin(), how[0].end(), std::ostream_iterator<std::string_view>(ss)); ss << "\n"; return ss; }; }
		if (Not(num[0]) == target) { done = true; func = [&]() { std::stringstream ss{}; ss << "~"; std::copy(how[0].begin(), how[0].end(), std::ostream_iterator<std::string_view>(ss)); ss << "\n"; return ss; }; }
		if (done /*&& std::regex_match(how[0], Re)*/)
		{
			SetQed();
			std::cout << func().rdbuf();
		}
		return;
	}
	for (decltype(num.size()) i = 0; i < num.size() - 1; ++i)
	{
		//for (auto j = i + 1; j < num.size(); ++j)

		{
			//if (i < j)
			{
				std::for_each(std::execution::par_unseq, Ops.begin(), Ops.end(), std::bind(SolveLoop, num, how, target, i, std::placeholders::_1));

			}
		}
	}
}

namespace Convert
{
	int ToInt(const std::string& value, const int base = 10)
	{
		int res;
		auto [p, e] = std::from_chars(value.data(), value.data() + value.length(), res, base);
		if (e != std::errc{}) throw std::runtime_error(std::string("convert error: invalid literal: ") + p);
		return res;
	}
}

int main(const int argc, char** argv)
{
	//step1();
	//step2();
	
	//for (int target = Convert::ToInt(argv[1]); target < 114515; ++target)
	//{
	//	Qed = false;
	//	std::cout << ">>" << target << "\n";
	//	std::for_each(std::execution::par_unseq, S1d.begin(), S1d.end(),
	//		[&](const std::vector<int>& num)
	//		{
	//			std::vector<std::vector<std::string_view>> how{};
	//			std::transform(num.begin(), num.end(), std::back_inserter(how),
	//				[](const auto x) -> std::vector<std::string_view> { return { NumStringPool.at(x) }; });
	//			Solve(num, how, target);
	//			//std::ostringstream ss{};
	//			//ss << "\t";
	//			//for (const auto& value : num)
	//			//{
	//			//	ss << value << " ";
	//			//}
	//			//ss << "\n";
	//			//std::cout << ss.str();
	//		});
	//}
	
	while (true)
	{
		Qed = false;
		int target;
		std::cout << ">>";
		std::cin >> target;
		std::for_each(std::execution::par_unseq, S1d.begin(), S1d.end(),
			[&](const std::vector<int>& num)
			{
				std::vector<std::vector<std::string_view>> how{};
				std::transform(num.begin(), num.end(), std::back_inserter(how), [](const auto x) -> std::vector<std::string_view> { return { NumStringPool.at(x) }; });
				Solve(num, how, target);
				std::ostringstream ss{};
				ss << "\t";
				for (const auto& value : num)
				{
					ss << value << " ";
				}
				ss << "\n";
				std::cout << ss.str();
			});
	}
}
