{-# LANGUAGE OverloadedStrings #-}
module PdfKit.AfmFont.Courier where

import PdfKit.AfmFont.AfmFont

afmFontCourier :: AfmFont
afmFontCourier = AfmFont
  { afmFontFontName = "Courier"
  , afmFontFullName = "Courier"
  , afmFontFamilyName = "Courier"
  , afmFontWeight = "Medium"
  , afmFontItalicAngle = 0
  , afmFontIsFixedPitch = True
  , afmFontCharacterSet = "ExtendedRoman"
  , afmFontFontBBox = (-23, -250, 715, 805)
  , afmFontUnderlinePosition = -100
  , afmFontUnderlineThickness = 50
  , afmFontVersion = "003.000"
  , afmFontEncodingScheme = "AdobeStandardEncoding"
  , afmFontCapHeight = Just 562
  , afmFontXHeight = Just 426
  , afmFontAscender = Just 629
  , afmFontDescender = Just (-157)
  , afmFontStdHW = 51
  , afmFontStdVW = 51
  , afmFontCharMetrics =
    [ AfmCharMetric 32 600 "space" (0, 0, 0, 0) []
    , AfmCharMetric 33 600 "exclam" (236, -15, 364, 572) []
    , AfmCharMetric 34 600 "quotedbl" (187, 328, 413, 562) []
    , AfmCharMetric 35 600 "numbersign" (93, -32, 507, 639) []
    , AfmCharMetric 36 600 "dollar" (105, -126, 496, 662) []
    , AfmCharMetric 37 600 "percent" (81, -15, 518, 622) []
    , AfmCharMetric 38 600 "ampersand" (63, -15, 538, 543) []
    , AfmCharMetric 39 600 "quoteright" (213, 328, 376, 562) []
    , AfmCharMetric 40 600 "parenleft" (269, -108, 440, 622) []
    , AfmCharMetric 41 600 "parenright" (160, -108, 331, 622) []
    , AfmCharMetric 42 600 "asterisk" (116, 257, 484, 607) []
    , AfmCharMetric 43 600 "plus" (80, 44, 520, 470) []
    , AfmCharMetric 44 600 "comma" (181, -112, 344, 122) []
    , AfmCharMetric 45 600 "hyphen" (103, 231, 497, 285) []
    , AfmCharMetric 46 600 "period" (229, -15, 371, 109) []
    , AfmCharMetric 47 600 "slash" (125, -80, 475, 629) []
    , AfmCharMetric 48 600 "zero" (106, -15, 494, 622) []
    , AfmCharMetric 49 600 "one" (96, 0, 505, 622) []
    , AfmCharMetric 50 600 "two" (70, 0, 471, 622) []
    , AfmCharMetric 51 600 "three" (75, -15, 466, 622) []
    , AfmCharMetric 52 600 "four" (78, 0, 500, 622) []
    , AfmCharMetric 53 600 "five" (92, -15, 497, 607) []
    , AfmCharMetric 54 600 "six" (111, -15, 497, 622) []
    , AfmCharMetric 55 600 "seven" (82, 0, 483, 607) []
    , AfmCharMetric 56 600 "eight" (102, -15, 498, 622) []
    , AfmCharMetric 57 600 "nine" (96, -15, 489, 622) []
    , AfmCharMetric 58 600 "colon" (229, -15, 371, 385) []
    , AfmCharMetric 59 600 "semicolon" (181, -112, 371, 385) []
    , AfmCharMetric 60 600 "less" (41, 42, 519, 472) []
    , AfmCharMetric 61 600 "equal" (80, 138, 520, 376) []
    , AfmCharMetric 62 600 "greater" (66, 42, 544, 472) []
    , AfmCharMetric 63 600 "question" (129, -15, 492, 572) []
    , AfmCharMetric 64 600 "at" (77, -15, 533, 622) []
    , AfmCharMetric 65 600 "A" (3, 0, 597, 562) []
    , AfmCharMetric 66 600 "B" (43, 0, 559, 562) []
    , AfmCharMetric 67 600 "C" (41, -18, 540, 580) []
    , AfmCharMetric 68 600 "D" (43, 0, 574, 562) []
    , AfmCharMetric 69 600 "E" (53, 0, 550, 562) []
    , AfmCharMetric 70 600 "F" (53, 0, 545, 562) []
    , AfmCharMetric 71 600 "G" (31, -18, 575, 580) []
    , AfmCharMetric 72 600 "H" (32, 0, 568, 562) []
    , AfmCharMetric 73 600 "I" (96, 0, 504, 562) []
    , AfmCharMetric 74 600 "J" (34, -18, 566, 562) []
    , AfmCharMetric 75 600 "K" (38, 0, 582, 562) []
    , AfmCharMetric 76 600 "L" (47, 0, 554, 562) []
    , AfmCharMetric 77 600 "M" (4, 0, 596, 562) []
    , AfmCharMetric 78 600 "N" (7, -13, 593, 562) []
    , AfmCharMetric 79 600 "O" (43, -18, 557, 580) []
    , AfmCharMetric 80 600 "P" (79, 0, 558, 562) []
    , AfmCharMetric 81 600 "Q" (43, -138, 557, 580) []
    , AfmCharMetric 82 600 "R" (38, 0, 588, 562) []
    , AfmCharMetric 83 600 "S" (72, -20, 529, 580) []
    , AfmCharMetric 84 600 "T" (38, 0, 563, 562) []
    , AfmCharMetric 85 600 "U" (17, -18, 583, 562) []
    , AfmCharMetric 86 600 "V" (-4, -13, 604, 562) []
    , AfmCharMetric 87 600 "W" (-3, -13, 603, 562) []
    , AfmCharMetric 88 600 "X" (23, 0, 577, 562) []
    , AfmCharMetric 89 600 "Y" (24, 0, 576, 562) []
    , AfmCharMetric 90 600 "Z" (86, 0, 514, 562) []
    , AfmCharMetric 91 600 "bracketleft" (269, -108, 442, 622) []
    , AfmCharMetric 92 600 "backslash" (118, -80, 482, 629) []
    , AfmCharMetric 93 600 "bracketright" (158, -108, 331, 622) []
    , AfmCharMetric 94 600 "asciicircum" (94, 354, 506, 622) []
    , AfmCharMetric 95 600 "underscore" (0, -125, 600, -75) []
    , AfmCharMetric 96 600 "quoteleft" (224, 328, 387, 562) []
    , AfmCharMetric 97 600 "a" (53, -15, 559, 441) []
    , AfmCharMetric 98 600 "b" (14, -15, 575, 629) []
    , AfmCharMetric 99 600 "c" (66, -15, 529, 441) []
    , AfmCharMetric 100 600 "d" (45, -15, 591, 629) []
    , AfmCharMetric 101 600 "e" (66, -15, 548, 441) []
    , AfmCharMetric 102 600 "f" (114, 0, 531, 629) [ ("i", "fi"), ("l", "fl")]
    , AfmCharMetric 103 600 "g" (45, -157, 566, 441) []
    , AfmCharMetric 104 600 "h" (18, 0, 582, 629) []
    , AfmCharMetric 105 600 "i" (95, 0, 505, 657) []
    , AfmCharMetric 106 600 "j" (82, -157, 410, 657) []
    , AfmCharMetric 107 600 "k" (43, 0, 580, 629) []
    , AfmCharMetric 108 600 "l" (95, 0, 505, 629) []
    , AfmCharMetric 109 600 "m" (-5, 0, 605, 441) []
    , AfmCharMetric 110 600 "n" (26, 0, 575, 441) []
    , AfmCharMetric 111 600 "o" (62, -15, 538, 441) []
    , AfmCharMetric 112 600 "p" (9, -157, 555, 441) []
    , AfmCharMetric 113 600 "q" (45, -157, 591, 441) []
    , AfmCharMetric 114 600 "r" (60, 0, 559, 441) []
    , AfmCharMetric 115 600 "s" (80, -15, 513, 441) []
    , AfmCharMetric 116 600 "t" (87, -15, 530, 561) []
    , AfmCharMetric 117 600 "u" (21, -15, 562, 426) []
    , AfmCharMetric 118 600 "v" (10, -10, 590, 426) []
    , AfmCharMetric 119 600 "w" (-4, -10, 604, 426) []
    , AfmCharMetric 120 600 "x" (20, 0, 580, 426) []
    , AfmCharMetric 121 600 "y" (7, -157, 592, 426) []
    , AfmCharMetric 122 600 "z" (99, 0, 502, 426) []
    , AfmCharMetric 123 600 "braceleft" (182, -108, 437, 622) []
    , AfmCharMetric 124 600 "bar" (275, -250, 326, 750) []
    , AfmCharMetric 125 600 "braceright" (163, -108, 418, 622) []
    , AfmCharMetric 126 600 "asciitilde" (63, 197, 540, 320) []
    , AfmCharMetric 161 600 "exclamdown" (236, -157, 364, 430) []
    , AfmCharMetric 162 600 "cent" (96, -49, 500, 614) []
    , AfmCharMetric 163 600 "sterling" (84, -21, 521, 611) []
    , AfmCharMetric 164 600 "fraction" (92, -57, 509, 665) []
    , AfmCharMetric 165 600 "yen" (26, 0, 574, 562) []
    , AfmCharMetric 166 600 "florin" (4, -143, 539, 622) []
    , AfmCharMetric 167 600 "section" (113, -78, 488, 580) []
    , AfmCharMetric 168 600 "currency" (73, 58, 527, 506) []
    , AfmCharMetric 169 600 "quotesingle" (259, 328, 341, 562) []
    , AfmCharMetric 170 600 "quotedblleft" (143, 328, 471, 562) []
    , AfmCharMetric 171 600 "guillemotleft" (37, 70, 563, 446) []
    , AfmCharMetric 172 600 "guilsinglleft" (149, 70, 451, 446) []
    , AfmCharMetric 173 600 "guilsinglright" (149, 70, 451, 446) []
    , AfmCharMetric 174 600 "fi" (3, 0, 597, 629) []
    , AfmCharMetric 175 600 "fl" (3, 0, 597, 629) []
    , AfmCharMetric 177 600 "endash" (75, 231, 525, 285) []
    , AfmCharMetric 178 600 "dagger" (141, -78, 459, 580) []
    , AfmCharMetric 179 600 "daggerdbl" (141, -78, 459, 580) []
    , AfmCharMetric 180 600 "periodcentered" (222, 189, 378, 327) []
    , AfmCharMetric 182 600 "paragraph" (50, -78, 511, 562) []
    , AfmCharMetric 183 600 "bullet" (172, 130, 428, 383) []
    , AfmCharMetric 184 600 "quotesinglbase" (213, -134, 376, 100) []
    , AfmCharMetric 185 600 "quotedblbase" (143, -134, 457, 100) []
    , AfmCharMetric 186 600 "quotedblright" (143, 328, 457, 562) []
    , AfmCharMetric 187 600 "guillemotright" (37, 70, 563, 446) []
    , AfmCharMetric 188 600 "ellipsis" (37, -15, 563, 111) []
    , AfmCharMetric 189 600 "perthousand" (3, -15, 600, 622) []
    , AfmCharMetric 191 600 "questiondown" (108, -157, 471, 430) []
    , AfmCharMetric 193 600 "grave" (151, 497, 378, 672) []
    , AfmCharMetric 194 600 "acute" (242, 497, 469, 672) []
    , AfmCharMetric 195 600 "circumflex" (124, 477, 476, 654) []
    , AfmCharMetric 196 600 "tilde" (105, 489, 503, 606) []
    , AfmCharMetric 197 600 "macron" (120, 525, 480, 565) []
    , AfmCharMetric 198 600 "breve" (153, 501, 447, 609) []
    , AfmCharMetric 199 600 "dotaccent" (249, 537, 352, 640) []
    , AfmCharMetric 200 600 "dieresis" (148, 537, 453, 640) []
    , AfmCharMetric 202 600 "ring" (218, 463, 382, 627) []
    , AfmCharMetric 203 600 "cedilla" (224, -151, 362, 10) []
    , AfmCharMetric 205 600 "hungarumlaut" (133, 497, 540, 672) []
    , AfmCharMetric 206 600 "ogonek" (211, -172, 407, 4) []
    , AfmCharMetric 207 600 "caron" (124, 492, 476, 669) []
    , AfmCharMetric 208 600 "emdash" (0, 231, 600, 285) []
    , AfmCharMetric 225 600 "AE" (3, 0, 550, 562) []
    , AfmCharMetric 227 600 "ordfeminine" (156, 249, 442, 580) []
    , AfmCharMetric 232 600 "Lslash" (47, 0, 554, 562) []
    , AfmCharMetric 233 600 "Oslash" (43, -80, 557, 629) []
    , AfmCharMetric 234 600 "OE" (7, 0, 567, 562) []
    , AfmCharMetric 235 600 "ordmasculine" (157, 249, 443, 580) []
    , AfmCharMetric 241 600 "ae" (19, -15, 570, 441) []
    , AfmCharMetric 245 600 "dotlessi" (95, 0, 505, 426) []
    , AfmCharMetric 248 600 "lslash" (95, 0, 505, 629) []
    , AfmCharMetric 249 600 "oslash" (62, -80, 538, 506) []
    , AfmCharMetric 250 600 "oe" (19, -15, 559, 441) []
    , AfmCharMetric 251 600 "germandbls" (48, -15, 588, 629) []
    , AfmCharMetric (-1) 600 "Idieresis" (96, 0, 504, 753) []
    , AfmCharMetric (-1) 600 "eacute" (66, -15, 548, 672) []
    , AfmCharMetric (-1) 600 "abreve" (53, -15, 559, 609) []
    , AfmCharMetric (-1) 600 "uhungarumlaut" (21, -15, 580, 672) []
    , AfmCharMetric (-1) 600 "ecaron" (66, -15, 548, 669) []
    , AfmCharMetric (-1) 600 "Ydieresis" (24, 0, 576, 753) []
    , AfmCharMetric (-1) 600 "divide" (87, 48, 513, 467) []
    , AfmCharMetric (-1) 600 "Yacute" (24, 0, 576, 805) []
    , AfmCharMetric (-1) 600 "Acircumflex" (3, 0, 597, 787) []
    , AfmCharMetric (-1) 600 "aacute" (53, -15, 559, 672) []
    , AfmCharMetric (-1) 600 "Ucircumflex" (17, -18, 583, 787) []
    , AfmCharMetric (-1) 600 "yacute" (7, -157, 592, 672) []
    , AfmCharMetric (-1) 600 "scommaaccent" (80, -250, 513, 441) []
    , AfmCharMetric (-1) 600 "ecircumflex" (66, -15, 548, 654) []
    , AfmCharMetric (-1) 600 "Uring" (17, -18, 583, 760) []
    , AfmCharMetric (-1) 600 "Udieresis" (17, -18, 583, 753) []
    , AfmCharMetric (-1) 600 "aogonek" (53, -172, 587, 441) []
    , AfmCharMetric (-1) 600 "Uacute" (17, -18, 583, 805) []
    , AfmCharMetric (-1) 600 "uogonek" (21, -172, 590, 426) []
    , AfmCharMetric (-1) 600 "Edieresis" (53, 0, 550, 753) []
    , AfmCharMetric (-1) 600 "Dcroat" (30, 0, 574, 562) []
    , AfmCharMetric (-1) 600 "commaaccent" (198, -250, 335, -58) []
    , AfmCharMetric (-1) 600 "copyright" (0, -18, 600, 580) []
    , AfmCharMetric (-1) 600 "Emacron" (53, 0, 550, 698) []
    , AfmCharMetric (-1) 600 "ccaron" (66, -15, 529, 669) []
    , AfmCharMetric (-1) 600 "aring" (53, -15, 559, 627) []
    , AfmCharMetric (-1) 600 "Ncommaaccent" (7, -250, 593, 562) []
    , AfmCharMetric (-1) 600 "lacute" (95, 0, 505, 805) []
    , AfmCharMetric (-1) 600 "agrave" (53, -15, 559, 672) []
    , AfmCharMetric (-1) 600 "Tcommaaccent" (38, -250, 563, 562) []
    , AfmCharMetric (-1) 600 "Cacute" (41, -18, 540, 805) []
    , AfmCharMetric (-1) 600 "atilde" (53, -15, 559, 606) []
    , AfmCharMetric (-1) 600 "Edotaccent" (53, 0, 550, 753) []
    , AfmCharMetric (-1) 600 "scaron" (80, -15, 513, 669) []
    , AfmCharMetric (-1) 600 "scedilla" (80, -151, 513, 441) []
    , AfmCharMetric (-1) 600 "iacute" (95, 0, 505, 672) []
    , AfmCharMetric (-1) 600 "lozenge" (18, 0, 443, 706) []
    , AfmCharMetric (-1) 600 "Rcaron" (38, 0, 588, 802) []
    , AfmCharMetric (-1) 600 "Gcommaaccent" (31, -250, 575, 580) []
    , AfmCharMetric (-1) 600 "ucircumflex" (21, -15, 562, 654) []
    , AfmCharMetric (-1) 600 "acircumflex" (53, -15, 559, 654) []
    , AfmCharMetric (-1) 600 "Amacron" (3, 0, 597, 698) []
    , AfmCharMetric (-1) 600 "rcaron" (60, 0, 559, 669) []
    , AfmCharMetric (-1) 600 "ccedilla" (66, -151, 529, 441) []
    , AfmCharMetric (-1) 600 "Zdotaccent" (86, 0, 514, 753) []
    , AfmCharMetric (-1) 600 "Thorn" (79, 0, 538, 562) []
    , AfmCharMetric (-1) 600 "Omacron" (43, -18, 557, 698) []
    , AfmCharMetric (-1) 600 "Racute" (38, 0, 588, 805) []
    , AfmCharMetric (-1) 600 "Sacute" (72, -20, 529, 805) []
    , AfmCharMetric (-1) 600 "dcaron" (45, -15, 715, 629) []
    , AfmCharMetric (-1) 600 "Umacron" (17, -18, 583, 698) []
    , AfmCharMetric (-1) 600 "uring" (21, -15, 562, 627) []
    , AfmCharMetric (-1) 600 "threesuperior" (155, 240, 406, 622) []
    , AfmCharMetric (-1) 600 "Ograve" (43, -18, 557, 805) []
    , AfmCharMetric (-1) 600 "Agrave" (3, 0, 597, 805) []
    , AfmCharMetric (-1) 600 "Abreve" (3, 0, 597, 732) []
    , AfmCharMetric (-1) 600 "multiply" (87, 43, 515, 470) []
    , AfmCharMetric (-1) 600 "uacute" (21, -15, 562, 672) []
    , AfmCharMetric (-1) 600 "Tcaron" (38, 0, 563, 802) []
    , AfmCharMetric (-1) 600 "partialdiff" (17, -38, 459, 710) []
    , AfmCharMetric (-1) 600 "ydieresis" (7, -157, 592, 620) []
    , AfmCharMetric (-1) 600 "Nacute" (7, -13, 593, 805) []
    , AfmCharMetric (-1) 600 "icircumflex" (94, 0, 505, 654) []
    , AfmCharMetric (-1) 600 "Ecircumflex" (53, 0, 550, 787) []
    , AfmCharMetric (-1) 600 "adieresis" (53, -15, 559, 620) []
    , AfmCharMetric (-1) 600 "edieresis" (66, -15, 548, 620) []
    , AfmCharMetric (-1) 600 "cacute" (66, -15, 529, 672) []
    , AfmCharMetric (-1) 600 "nacute" (26, 0, 575, 672) []
    , AfmCharMetric (-1) 600 "umacron" (21, -15, 562, 565) []
    , AfmCharMetric (-1) 600 "Ncaron" (7, -13, 593, 802) []
    , AfmCharMetric (-1) 600 "Iacute" (96, 0, 504, 805) []
    , AfmCharMetric (-1) 600 "plusminus" (87, 44, 513, 558) []
    , AfmCharMetric (-1) 600 "brokenbar" (275, -175, 326, 675) []
    , AfmCharMetric (-1) 600 "registered" (0, -18, 600, 580) []
    , AfmCharMetric (-1) 600 "Gbreve" (31, -18, 575, 732) []
    , AfmCharMetric (-1) 600 "Idotaccent" (96, 0, 504, 753) []
    , AfmCharMetric (-1) 600 "summation" (15, -10, 585, 706) []
    , AfmCharMetric (-1) 600 "Egrave" (53, 0, 550, 805) []
    , AfmCharMetric (-1) 600 "racute" (60, 0, 559, 672) []
    , AfmCharMetric (-1) 600 "omacron" (62, -15, 538, 565) []
    , AfmCharMetric (-1) 600 "Zacute" (86, 0, 514, 805) []
    , AfmCharMetric (-1) 600 "Zcaron" (86, 0, 514, 802) []
    , AfmCharMetric (-1) 600 "greaterequal" (98, 0, 502, 710) []
    , AfmCharMetric (-1) 600 "Eth" (30, 0, 574, 562) []
    , AfmCharMetric (-1) 600 "Ccedilla" (41, -151, 540, 580) []
    , AfmCharMetric (-1) 600 "lcommaaccent" (95, -250, 505, 629) []
    , AfmCharMetric (-1) 600 "tcaron" (87, -15, 530, 717) []
    , AfmCharMetric (-1) 600 "eogonek" (66, -172, 548, 441) []
    , AfmCharMetric (-1) 600 "Uogonek" (17, -172, 583, 562) []
    , AfmCharMetric (-1) 600 "Aacute" (3, 0, 597, 805) []
    , AfmCharMetric (-1) 600 "Adieresis" (3, 0, 597, 753) []
    , AfmCharMetric (-1) 600 "egrave" (66, -15, 548, 672) []
    , AfmCharMetric (-1) 600 "zacute" (99, 0, 502, 672) []
    , AfmCharMetric (-1) 600 "iogonek" (95, -172, 505, 657) []
    , AfmCharMetric (-1) 600 "Oacute" (43, -18, 557, 805) []
    , AfmCharMetric (-1) 600 "oacute" (62, -15, 538, 672) []
    , AfmCharMetric (-1) 600 "amacron" (53, -15, 559, 565) []
    , AfmCharMetric (-1) 600 "sacute" (80, -15, 513, 672) []
    , AfmCharMetric (-1) 600 "idieresis" (95, 0, 505, 620) []
    , AfmCharMetric (-1) 600 "Ocircumflex" (43, -18, 557, 787) []
    , AfmCharMetric (-1) 600 "Ugrave" (17, -18, 583, 805) []
    , AfmCharMetric (-1) 600 "Delta" (6, 0, 598, 688) []
    , AfmCharMetric (-1) 600 "thorn" (-6, -157, 555, 629) []
    , AfmCharMetric (-1) 600 "twosuperior" (177, 249, 424, 622) []
    , AfmCharMetric (-1) 600 "Odieresis" (43, -18, 557, 753) []
    , AfmCharMetric (-1) 600 "mu" (21, -157, 562, 426) []
    , AfmCharMetric (-1) 600 "igrave" (95, 0, 505, 672) []
    , AfmCharMetric (-1) 600 "ohungarumlaut" (62, -15, 580, 672) []
    , AfmCharMetric (-1) 600 "Eogonek" (53, -172, 561, 562) []
    , AfmCharMetric (-1) 600 "dcroat" (45, -15, 591, 629) []
    , AfmCharMetric (-1) 600 "threequarters" (8, -56, 593, 666) []
    , AfmCharMetric (-1) 600 "Scedilla" (72, -151, 529, 580) []
    , AfmCharMetric (-1) 600 "lcaron" (95, 0, 533, 629) []
    , AfmCharMetric (-1) 600 "Kcommaaccent" (38, -250, 582, 562) []
    , AfmCharMetric (-1) 600 "Lacute" (47, 0, 554, 805) []
    , AfmCharMetric (-1) 600 "trademark" (-23, 263, 623, 562) []
    , AfmCharMetric (-1) 600 "edotaccent" (66, -15, 548, 620) []
    , AfmCharMetric (-1) 600 "Igrave" (96, 0, 504, 805) []
    , AfmCharMetric (-1) 600 "Imacron" (96, 0, 504, 698) []
    , AfmCharMetric (-1) 600 "Lcaron" (47, 0, 554, 562) []
    , AfmCharMetric (-1) 600 "onehalf" (0, -57, 611, 665) []
    , AfmCharMetric (-1) 600 "lessequal" (98, 0, 502, 710) []
    , AfmCharMetric (-1) 600 "ocircumflex" (62, -15, 538, 654) []
    , AfmCharMetric (-1) 600 "ntilde" (26, 0, 575, 606) []
    , AfmCharMetric (-1) 600 "Uhungarumlaut" (17, -18, 590, 805) []
    , AfmCharMetric (-1) 600 "Eacute" (53, 0, 550, 805) []
    , AfmCharMetric (-1) 600 "emacron" (66, -15, 548, 565) []
    , AfmCharMetric (-1) 600 "gbreve" (45, -157, 566, 609) []
    , AfmCharMetric (-1) 600 "onequarter" (0, -57, 600, 665) []
    , AfmCharMetric (-1) 600 "Scaron" (72, -20, 529, 802) []
    , AfmCharMetric (-1) 600 "Scommaaccent" (72, -250, 529, 580) []
    , AfmCharMetric (-1) 600 "Ohungarumlaut" (43, -18, 580, 805) []
    , AfmCharMetric (-1) 600 "degree" (123, 269, 477, 622) []
    , AfmCharMetric (-1) 600 "ograve" (62, -15, 538, 672) []
    , AfmCharMetric (-1) 600 "Ccaron" (41, -18, 540, 802) []
    , AfmCharMetric (-1) 600 "ugrave" (21, -15, 562, 672) []
    , AfmCharMetric (-1) 600 "radical" (3, -15, 597, 792) []
    , AfmCharMetric (-1) 600 "Dcaron" (43, 0, 574, 802) []
    , AfmCharMetric (-1) 600 "rcommaaccent" (60, -250, 559, 441) []
    , AfmCharMetric (-1) 600 "Ntilde" (7, -13, 593, 729) []
    , AfmCharMetric (-1) 600 "otilde" (62, -15, 538, 606) []
    , AfmCharMetric (-1) 600 "Rcommaaccent" (38, -250, 588, 562) []
    , AfmCharMetric (-1) 600 "Lcommaaccent" (47, -250, 554, 562) []
    , AfmCharMetric (-1) 600 "Atilde" (3, 0, 597, 729) []
    , AfmCharMetric (-1) 600 "Aogonek" (3, -172, 608, 562) []
    , AfmCharMetric (-1) 600 "Aring" (3, 0, 597, 750) []
    , AfmCharMetric (-1) 600 "Otilde" (43, -18, 557, 729) []
    , AfmCharMetric (-1) 600 "zdotaccent" (99, 0, 502, 620) []
    , AfmCharMetric (-1) 600 "Ecaron" (53, 0, 550, 802) []
    , AfmCharMetric (-1) 600 "Iogonek" (96, -172, 504, 562) []
    , AfmCharMetric (-1) 600 "kcommaaccent" (43, -250, 580, 629) []
    , AfmCharMetric (-1) 600 "minus" (80, 232, 520, 283) []
    , AfmCharMetric (-1) 600 "Icircumflex" (96, 0, 504, 787) []
    , AfmCharMetric (-1) 600 "ncaron" (26, 0, 575, 669) []
    , AfmCharMetric (-1) 600 "tcommaaccent" (87, -250, 530, 561) []
    , AfmCharMetric (-1) 600 "logicalnot" (87, 108, 513, 369) []
    , AfmCharMetric (-1) 600 "odieresis" (62, -15, 538, 620) []
    , AfmCharMetric (-1) 600 "udieresis" (21, -15, 562, 620) []
    , AfmCharMetric (-1) 600 "notequal" (15, -16, 540, 529) []
    , AfmCharMetric (-1) 600 "gcommaaccent" (45, -157, 566, 708) []
    , AfmCharMetric (-1) 600 "eth" (62, -15, 538, 629) []
    , AfmCharMetric (-1) 600 "zcaron" (99, 0, 502, 669) []
    , AfmCharMetric (-1) 600 "ncommaaccent" (26, -250, 575, 441) []
    , AfmCharMetric (-1) 600 "onesuperior" (172, 249, 428, 622) []
    , AfmCharMetric (-1) 600 "imacron" (95, 0, 505, 565) []
    , AfmCharMetric (-1) 600 "Euro" (0, 0, 0, 0) []
    ]
  , afmFontKernPairXs =
    [ 
    ]
  }
