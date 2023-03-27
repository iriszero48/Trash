#include <iostream>
#include <fstream>
#include <string>
#include <filesystem>

#include <nlohmann/json.hpp>

int main(int argc, char **argv)
{
    if (argc != 3)
    {
        std::cerr << "argc != 3 which argc = " << argc << std::endl;
        std::cout << "Usage: " << argv[0] << " prefix file_path" << std::endl;
        exit(1);
    }

    const auto prefix = std::filesystem::path(argv[1]).u8string();
    const std::string file_path = argv[2];

    const auto key_str = prefix + u8"_原始值";
    std::string_view key((const char *)key_str.data(), key_str.length());

    std::ifstream fs(file_path, std::ios::in | std::ios::binary);
    std::string line;
    bool exit = false;
    while (!exit)
    {
        std::getline(fs, line);
        if (!fs)
            exit = true;

        if (!line.empty())
        {
            std::cout
                << nlohmann::json({{key, line}}).dump() << "\n";
        }
    }

    fs.close();
}
