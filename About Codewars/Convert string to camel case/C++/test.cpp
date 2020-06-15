Describe(any_group_name_you_want)
{
    It(should_do_something)
    {
        Assert::That(to_camel_case("the-stealth-warrior"), Equals("theStealthWarrior"));
        Assert::That(to_camel_case("The_Stealth_Warrior"), Equals("TheStealthWarrior"));
    }
};
