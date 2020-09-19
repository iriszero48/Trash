{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Reader
import Data.List
import System.Environment
import TeX

basicInfo :: Resume
basicInfo = paragraph
  [ pure "\\basicInfo{"
  , pure $ "\\email{ih@iriszero.cc}" ++ period
  -- , cn $ "\\phone{(+86) 000-0000-0000}" ++ period
  , pure $ "\\github[iriszero48]{https://github.com/iriszero48}" ++ period
  , pure $ "\\homepage[iriszero.top]{https://iriszero.top}"
  , pure "}"
  ] where period = "\\textperiodcentered\\"

education :: Resume
education = section "教育经历" "Education"
  [ datedSection (date "2017" "09" ~~ date "2021" "06") $ paragraph
    [ en "\\textbf{Swan college of Central South University of Forestry and Technology}"
    , cn "\\textbf{中南林业科技大学涉外学院}"
    ]
  , en $ "Major: Computer Science and Technology (Undergraduate)"
  , cn $ "专业：计算机科学与技术（本科）"
  ]

sourcebrella :: Resume
sourcebrella = paragraph
  [ paragraph [ cn "\\textbf{国赛}" ]
  , itemize
    [ cn "\\item 2018年7月，第十一届全国大学生信息安全竞赛创新能力实践赛全国三等奖；"
    , cn "\\item 2019年5月，第十届蓝桥杯全国软件和信息技术专业人才大赛全国总决赛Java软件开发大学B组优秀奖；"
    , cn "\\item 2019年6月，第十二届全国大学生信息安全竞赛创新实践能力赛华中赛区二等奖；"
    ]
  , paragraph [ cn "\\textbf{省赛}" ]
  , itemize
    [ cn "\\item 2018年4月，第九届蓝桥杯全国软件和信息技术专业人才大赛湖南赛区Java软件开发大学B组三等奖；"
    , cn "\\item 2018年12月，湖南省第二届网络安全技能竞赛三等奖；"
    , cn "\\item 2019年3月，第十届蓝桥杯全国软件和信息技术专业人才大赛湖南赛区Java软件开发大学B组一等奖；"
    , cn "\\item 2019年6月，湖南省首届高校白帽子邀请赛二等奖；"
    , cn "\\item 2019年12月，湖南省第三届CRH杂交水稻杯机器人创客大赛一等奖。"
    ]
  , paragraph [ cn "\\textbf{校赛}" ]
  , itemize
    [ cn "\\item 2018年7月，中南林业科技大学涉外学院第一届大学生信息安全竞赛二等奖；"
    , cn "\\item 2018年10月，中南林业科技大学涉外学院第一届程序设计大赛三等奖；"
    , cn "\\item 2018年12月，中南林业科技大学涉外学院第二届程序设计大赛三等奖；"
    , cn "\\item 2019年6月，中南林业科技大学涉外学院第三届程序设计大赛二等奖；"
    , cn "\\item 2019年6月，中南林业科技大学涉外学院第二届大学生信息安全竞赛 二等奖；"
    , cn "\\item 2019年12月，中南林业科技大学涉外学院第四届程序设计大赛二等奖；"
    ]
  , paragraph [ cn "\\textbf{其他}" ]
  , itemize
    [ cn "\\item 2019年11月，第二届湖湘杯网络安全技能大赛“逆向”出题人"
    ]
  ]

httpAutoIndexServer :: Resume
httpAutoIndexServer = paragraph
  [ github "iriszero48/HttpAutoIndexServer" `datedSection` bold "Http Auto Index Server"
  , cn "Windows/Linux C++ 多线程 自动索引文件HTTP服务器"
  , en "Auto Index HTTP Server"

  , itemize
    [ cn "\\item V1.x 使用C++11"
    , en "\\item V1.x uses C++11"

    , cn "\\item V2.x 使用C++17，EPOLL/IOCP"
    , en "\\item V2.x uses C++17, EPOLL/IOCP"

    , cn "\\item 使用vs/cmake管理工程"
    , en "\\item Uses vs/cmake"
    ]
  ]

conwaysGameOfLife :: Resume
conwaysGameOfLife = paragraph
  [ github "iriszero48/Conway-s-Game-of-Life" `datedSection` bold "Conway's Game of Life"
  , cn $ "C++ WindowsGDI 康威生命游戏"
  , en $ "C++ WindowsGDI Conway's Game of Life"
  ]

devTools :: Resume
devTools = paragraph
  [ itemTeX "开发工具" "Development Tool"
  , cn $ "常用" ++ intercalate "、 " tools
  ] where
     tools = ["Visual Studio", "JetBrains", "GitHub", "nano", "vim", "Sublime text"]

skills :: Resume
skills = section "技能" "Skills" . pure $ itemize
  [ itemTeX "编程语言" "Program Language"
  , cn "能快速学习一门新语言，"
  , cn $ "熟悉 " ++ lang
  , en "\\textbf{multilingual},"
  , en $ "experienced in " ++ lang

  , itemTeX "并发网络模型" "Network Model"
  , cn $ "能用C++使用常用的网络模型 " ++ modle

  , itemTeX "操作系统" "Operating System"
  , cn $ "熟悉 " ++ system ++ "，有编译安装amd64/armhf的Linux经验"

  , devTools
  ] where
      lang = "C\\# C C++ F\\# Java HTML CSS JavaScript Python Bash"
      modle = "EPOLL IOCP Select ASIO"
      system  = "Windows Ubuntu Debian Kali Gentoo"

resume :: Resume
resume = paragraph
  [ pure "% !TEX program = xelatex"
  , pure "% This file is generated, don't manually edit!"

  -- head
  , paragraph
    [ pure "\\documentclass{resume}"
    , cn   "\\usepackage{lastpage}"
    , cn   "\\usepackage{fancyhdr}"
    -- disable extra space before next section
    , pure "\\usepackage{linespacing_fix}"
    , cn   "\\usepackage[fallback]{xeCJK}"
    ]

  --  \setmainfont[]{SimSun}
  --  \setCJKfallbackfamilyfont{rm}{HAN NOM B}
  --  \setCJKmainfont{Source Han Serif SC Regular}
  --  \renewcommand{\thepage}{\Chinese{page}}

  -- begin document
  , pure "\\begin{document}"
  , cn "\\renewcommand\\headrulewidth{0pt}"

  -- dare?
  , tex "name" "废物" "garbage"

  , basicInfo
  , education

  , section "个人经历" "Personal Experience"
    [ sourcebrella
    ]

  , section "个人项目" "Personal Projects"
    [ noSimple $ httpAutoIndexServer
    , noSimple $ conwaysGameOfLife
    ]

  , skills

  , section "其他" "Miscellaneous"
    [ itemize
      [ cn "\\item 喜欢音乐，常用FL Studio编曲"
      , cn "\\item 常用CINEMA 4D建模"
      , cn "\\item 常用FFmpeg、 Adobe、 Vapour Synth处理媒体"
      , cn "\\item 喜欢设计、 制作电路"
      ]
    ]

  -- end document
  , pure "\\end{document}"
  ]

main :: IO ()
main = getArgs >>= \case
  ["cn"]   -> f Chinese
  ["en"]   -> f English
  ["elab"] -> f Elaborated
  _        -> putStrLn $ unlines
    [ "Usage: <program> MODE"
    , "MODE can be cn, en or elab"
    ]
  where f = putStrLn . runReader resume
