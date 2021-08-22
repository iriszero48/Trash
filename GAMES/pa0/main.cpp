#include<cmath>
#include<eigen3/Eigen/Core>
#include<iostream>

template<typename T = float> constexpr T pi = T(3.1415926535897932385L);

int main()
{
    const auto rotate = [](const float deg)
    {
        const auto x = deg * pi<> / 180.f;
        return (Eigen::Matrix3f() <<
            cos(x), -sin(x), 0,
            sin(x),  cos(x), 0,
                 0,       0, 1).finished();
    };

    const auto translate = [](const float x, const float y)
    {
        return (Eigen::Matrix3f() <<
            1, 0, x,
            0, 1, y,
            0, 0, 1).finished();
    };

    const Eigen::Vector3f point(2, 1, 1);
    const auto transform = translate(1, 2) * rotate(45);
    Eigen::Vector3f res = transform * point;
    res /= res[2];
    std::cout << res.head<2>();
    return 0;
}
