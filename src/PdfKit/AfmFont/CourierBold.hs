{-# LANGUAGE OverloadedStrings #-}
module PdfKit.AfmFont.CourierBold where

import PdfKit.AfmFont.AfmFont

afmFontCourierBold :: AfmFont
afmFontCourierBold = AfmFont
  { afmFontFontName = "Courier-Bold"
  , afmFontFullName = "Courier Bold"
  , afmFontFamilyName = "Courier"
  , afmFontWeight = "Bold"
  , afmFontItalicAngle = 0
  , afmFontIsFixedPitch = True
  , afmFontCharacterSet = "ExtendedRoman"
  , afmFontFontBBox = (-113, -250, 749, 801)
  , afmFontUnderlinePosition = -100
  , afmFontUnderlineThickness = 50
  , afmFontVersion = "003.000"
  , afmFontEncodingScheme = "AdobeStandardEncoding"
  , afmFontCapHeight = Just 562
  , afmFontXHeight = Just 439
  , afmFontAscender = Just 629
  , afmFontDescender = Just (-157)
  , afmFontStdHW = 84
  , afmFontStdVW = 106
  , afmFontCharMetrics =
    [ AfmCharMetric 32 600 "space" (0, 0, 0, 0) []
    , AfmCharMetric 33 600 "exclam" (202, -15, 398, 572) []
    , AfmCharMetric 34 600 "quotedbl" (135, 277, 465, 562) []
    , AfmCharMetric 35 600 "numbersign" (56, -45, 544, 651) []
    , AfmCharMetric 36 600 "dollar" (82, -126, 519, 666) []
    , AfmCharMetric 37 600 "percent" (5, -15, 595, 616) []
    , AfmCharMetric 38 600 "ampersand" (36, -15, 546, 543) []
    , AfmCharMetric 39 600 "quoteright" (171, 277, 423, 562) []
    , AfmCharMetric 40 600 "parenleft" (219, -102, 461, 616) []
    , AfmCharMetric 41 600 "parenright" (139, -102, 381, 616) []
    , AfmCharMetric 42 600 "asterisk" (91, 219, 509, 601) []
    , AfmCharMetric 43 600 "plus" (71, 39, 529, 478) []
    , AfmCharMetric 44 600 "comma" (123, -111, 393, 174) []
    , AfmCharMetric 45 600 "hyphen" (100, 203, 500, 313) []
    , AfmCharMetric 46 600 "period" (192, -15, 408, 171) []
    , AfmCharMetric 47 600 "slash" (98, -77, 502, 626) []
    , AfmCharMetric 48 600 "zero" (87, -15, 513, 616) []
    , AfmCharMetric 49 600 "one" (81, 0, 539, 616) []
    , AfmCharMetric 50 600 "two" (61, 0, 499, 616) []
    , AfmCharMetric 51 600 "three" (63, -15, 501, 616) []
    , AfmCharMetric 52 600 "four" (53, 0, 507, 616) []
    , AfmCharMetric 53 600 "five" (70, -15, 521, 601) []
    , AfmCharMetric 54 600 "six" (90, -15, 521, 616) []
    , AfmCharMetric 55 600 "seven" (55, 0, 494, 601) []
    , AfmCharMetric 56 600 "eight" (83, -15, 517, 616) []
    , AfmCharMetric 57 600 "nine" (79, -15, 510, 616) []
    , AfmCharMetric 58 600 "colon" (191, -15, 407, 425) []
    , AfmCharMetric 59 600 "semicolon" (123, -111, 408, 425) []
    , AfmCharMetric 60 600 "less" (66, 15, 523, 501) []
    , AfmCharMetric 61 600 "equal" (71, 118, 529, 398) []
    , AfmCharMetric 62 600 "greater" (77, 15, 534, 501) []
    , AfmCharMetric 63 600 "question" (98, -14, 501, 580) []
    , AfmCharMetric 64 600 "at" (16, -15, 584, 616) []
    , AfmCharMetric 65 600 "A" (-9, 0, 609, 562) []
    , AfmCharMetric 66 600 "B" (30, 0, 573, 562) []
    , AfmCharMetric 67 600 "C" (22, -18, 560, 580) []
    , AfmCharMetric 68 600 "D" (30, 0, 594, 562) []
    , AfmCharMetric 69 600 "E" (25, 0, 560, 562) []
    , AfmCharMetric 70 600 "F" (39, 0, 570, 562) []
    , AfmCharMetric 71 600 "G" (22, -18, 594, 580) []
    , AfmCharMetric 72 600 "H" (20, 0, 580, 562) []
    , AfmCharMetric 73 600 "I" (77, 0, 523, 562) []
    , AfmCharMetric 74 600 "J" (37, -18, 601, 562) []
    , AfmCharMetric 75 600 "K" (21, 0, 599, 562) []
    , AfmCharMetric 76 600 "L" (39, 0, 578, 562) []
    , AfmCharMetric 77 600 "M" (-2, 0, 602, 562) []
    , AfmCharMetric 78 600 "N" (8, -12, 610, 562) []
    , AfmCharMetric 79 600 "O" (22, -18, 578, 580) []
    , AfmCharMetric 80 600 "P" (48, 0, 559, 562) []
    , AfmCharMetric 81 600 "Q" (32, -138, 578, 580) []
    , AfmCharMetric 82 600 "R" (24, 0, 599, 562) []
    , AfmCharMetric 83 600 "S" (47, -22, 553, 582) []
    , AfmCharMetric 84 600 "T" (21, 0, 579, 562) []
    , AfmCharMetric 85 600 "U" (4, -18, 596, 562) []
    , AfmCharMetric 86 600 "V" (-13, 0, 613, 562) []
    , AfmCharMetric 87 600 "W" (-18, 0, 618, 562) []
    , AfmCharMetric 88 600 "X" (12, 0, 588, 562) []
    , AfmCharMetric 89 600 "Y" (12, 0, 589, 562) []
    , AfmCharMetric 90 600 "Z" (62, 0, 539, 562) []
    , AfmCharMetric 91 600 "bracketleft" (245, -102, 475, 616) []
    , AfmCharMetric 92 600 "backslash" (99, -77, 503, 626) []
    , AfmCharMetric 93 600 "bracketright" (125, -102, 355, 616) []
    , AfmCharMetric 94 600 "asciicircum" (108, 250, 492, 616) []
    , AfmCharMetric 95 600 "underscore" (0, -125, 600, -75) []
    , AfmCharMetric 96 600 "quoteleft" (178, 277, 428, 562) []
    , AfmCharMetric 97 600 "a" (35, -15, 570, 454) []
    , AfmCharMetric 98 600 "b" (0, -15, 584, 626) []
    , AfmCharMetric 99 600 "c" (40, -15, 545, 459) []
    , AfmCharMetric 100 600 "d" (20, -15, 591, 626) []
    , AfmCharMetric 101 600 "e" (40, -15, 563, 454) []
    , AfmCharMetric 102 600 "f" (83, 0, 547, 626) [("i", "fi"), ("l", "fl")]
    , AfmCharMetric 103 600 "g" (30, -146, 580, 454) []
    , AfmCharMetric 104 600 "h" (5, 0, 592, 626) []
    , AfmCharMetric 105 600 "i" (77, 0, 523, 658) []
    , AfmCharMetric 106 600 "j" (63, -146, 440, 658) []
    , AfmCharMetric 107 600 "k" (20, 0, 585, 626) []
    , AfmCharMetric 108 600 "l" (77, 0, 523, 626) []
    , AfmCharMetric 109 600 "m" (-22, 0, 626, 454) []
    , AfmCharMetric 110 600 "n" (18, 0, 592, 454) []
    , AfmCharMetric 111 600 "o" (30, -15, 570, 454) []
    , AfmCharMetric 112 600 "p" (-1, -142, 570, 454) []
    , AfmCharMetric 113 600 "q" (20, -142, 591, 454) []
    , AfmCharMetric 114 600 "r" (47, 0, 580, 454) []
    , AfmCharMetric 115 600 "s" (68, -17, 535, 459) []
    , AfmCharMetric 116 600 "t" (47, -15, 532, 562) []
    , AfmCharMetric 117 600 "u" (-1, -15, 569, 439) []
    , AfmCharMetric 118 600 "v" (-1, 0, 601, 439) []
    , AfmCharMetric 119 600 "w" (-18, 0, 618, 439) []
    , AfmCharMetric 120 600 "x" (6, 0, 594, 439) []
    , AfmCharMetric 121 600 "y" (-4, -142, 601, 439) []
    , AfmCharMetric 122 600 "z" (81, 0, 520, 439) []
    , AfmCharMetric 123 600 "braceleft" (160, -102, 464, 616) []
    , AfmCharMetric 124 600 "bar" (255, -250, 345, 750) []
    , AfmCharMetric 125 600 "braceright" (136, -102, 440, 616) []
    , AfmCharMetric 126 600 "asciitilde" (71, 153, 530, 356) []
    , AfmCharMetric 161 600 "exclamdown" (202, -146, 398, 449) []
    , AfmCharMetric 162 600 "cent" (66, -49, 518, 614) []
    , AfmCharMetric 163 600 "sterling" (72, -28, 558, 611) []
    , AfmCharMetric 164 600 "fraction" (25, -60, 576, 661) []
    , AfmCharMetric 165 600 "yen" (10, 0, 590, 562) []
    , AfmCharMetric 166 600 "florin" (-30, -131, 572, 616) []
    , AfmCharMetric 167 600 "section" (83, -70, 517, 580) []
    , AfmCharMetric 168 600 "currency" (54, 49, 546, 517) []
    , AfmCharMetric 169 600 "quotesingle" (227, 277, 373, 562) []
    , AfmCharMetric 170 600 "quotedblleft" (71, 277, 535, 562) []
    , AfmCharMetric 171 600 "guillemotleft" (8, 70, 553, 446) []
    , AfmCharMetric 172 600 "guilsinglleft" (141, 70, 459, 446) []
    , AfmCharMetric 173 600 "guilsinglright" (141, 70, 459, 446) []
    , AfmCharMetric 174 600 "fi" (12, 0, 593, 626) []
    , AfmCharMetric 175 600 "fl" (12, 0, 593, 626) []
    , AfmCharMetric 177 600 "endash" (65, 203, 535, 313) []
    , AfmCharMetric 178 600 "dagger" (106, -70, 494, 580) []
    , AfmCharMetric 179 600 "daggerdbl" (106, -70, 494, 580) []
    , AfmCharMetric 180 600 "periodcentered" (196, 165, 404, 351) []
    , AfmCharMetric 182 600 "paragraph" (6, -70, 576, 580) []
    , AfmCharMetric 183 600 "bullet" (140, 132, 460, 430) []
    , AfmCharMetric 184 600 "quotesinglbase" (175, -142, 427, 143) []
    , AfmCharMetric 185 600 "quotedblbase" (65, -142, 529, 143) []
    , AfmCharMetric 186 600 "quotedblright" (61, 277, 525, 562) []
    , AfmCharMetric 187 600 "guillemotright" (47, 70, 592, 446) []
    , AfmCharMetric 188 600 "ellipsis" (26, -15, 574, 116) []
    , AfmCharMetric 189 600 "perthousand" (-113, -15, 713, 616) []
    , AfmCharMetric 191 600 "questiondown" (99, -146, 502, 449) []
    , AfmCharMetric 193 600 "grave" (132, 508, 395, 661) []
    , AfmCharMetric 194 600 "acute" (205, 508, 468, 661) []
    , AfmCharMetric 195 600 "circumflex" (103, 483, 497, 657) []
    , AfmCharMetric 196 600 "tilde" (89, 493, 512, 636) []
    , AfmCharMetric 197 600 "macron" (88, 505, 512, 585) []
    , AfmCharMetric 198 600 "breve" (83, 468, 517, 631) []
    , AfmCharMetric 199 600 "dotaccent" (230, 498, 370, 638) []
    , AfmCharMetric 200 600 "dieresis" (128, 498, 472, 638) []
    , AfmCharMetric 202 600 "ring" (198, 481, 402, 678) []
    , AfmCharMetric 203 600 "cedilla" (205, -206, 387, 0) []
    , AfmCharMetric 205 600 "hungarumlaut" (68, 488, 588, 661) []
    , AfmCharMetric 206 600 "ogonek" (169, -199, 400, 0) []
    , AfmCharMetric 207 600 "caron" (103, 493, 497, 667) []
    , AfmCharMetric 208 600 "emdash" (-10, 203, 610, 313) []
    , AfmCharMetric 225 600 "AE" (-29, 0, 602, 562) []
    , AfmCharMetric 227 600 "ordfeminine" (147, 196, 453, 580) []
    , AfmCharMetric 232 600 "Lslash" (39, 0, 578, 562) []
    , AfmCharMetric 233 600 "Oslash" (22, -22, 578, 584) []
    , AfmCharMetric 234 600 "OE" (-25, 0, 595, 562) []
    , AfmCharMetric 235 600 "ordmasculine" (147, 196, 453, 580) []
    , AfmCharMetric 241 600 "ae" (-4, -15, 601, 454) []
    , AfmCharMetric 245 600 "dotlessi" (77, 0, 523, 439) []
    , AfmCharMetric 248 600 "lslash" (77, 0, 523, 626) []
    , AfmCharMetric 249 600 "oslash" (30, -24, 570, 463) []
    , AfmCharMetric 250 600 "oe" (-18, -15, 611, 454) []
    , AfmCharMetric 251 600 "germandbls" (22, -15, 596, 626) []
    , AfmCharMetric (-1) 600 "Idieresis" (77, 0, 523, 761) []
    , AfmCharMetric (-1) 600 "eacute" (40, -15, 563, 661) []
    , AfmCharMetric (-1) 600 "abreve" (35, -15, 570, 661) []
    , AfmCharMetric (-1) 600 "uhungarumlaut" (-1, -15, 628, 661) []
    , AfmCharMetric (-1) 600 "ecaron" (40, -15, 563, 667) []
    , AfmCharMetric (-1) 600 "Ydieresis" (12, 0, 589, 761) []
    , AfmCharMetric (-1) 600 "divide" (71, 16, 529, 500) []
    , AfmCharMetric (-1) 600 "Yacute" (12, 0, 589, 784) []
    , AfmCharMetric (-1) 600 "Acircumflex" (-9, 0, 609, 780) []
    , AfmCharMetric (-1) 600 "aacute" (35, -15, 570, 661) []
    , AfmCharMetric (-1) 600 "Ucircumflex" (4, -18, 596, 780) []
    , AfmCharMetric (-1) 600 "yacute" (-4, -142, 601, 661) []
    , AfmCharMetric (-1) 600 "scommaaccent" (68, -250, 535, 459) []
    , AfmCharMetric (-1) 600 "ecircumflex" (40, -15, 563, 657) []
    , AfmCharMetric (-1) 600 "Uring" (4, -18, 596, 801) []
    , AfmCharMetric (-1) 600 "Udieresis" (4, -18, 596, 761) []
    , AfmCharMetric (-1) 600 "aogonek" (35, -199, 586, 454) []
    , AfmCharMetric (-1) 600 "Uacute" (4, -18, 596, 784) []
    , AfmCharMetric (-1) 600 "uogonek" (-1, -199, 585, 439) []
    , AfmCharMetric (-1) 600 "Edieresis" (25, 0, 560, 761) []
    , AfmCharMetric (-1) 600 "Dcroat" (30, 0, 594, 562) []
    , AfmCharMetric (-1) 600 "commaaccent" (205, -250, 397, -57) []
    , AfmCharMetric (-1) 600 "copyright" (0, -18, 600, 580) []
    , AfmCharMetric (-1) 600 "Emacron" (25, 0, 560, 708) []
    , AfmCharMetric (-1) 600 "ccaron" (40, -15, 545, 667) []
    , AfmCharMetric (-1) 600 "aring" (35, -15, 570, 678) []
    , AfmCharMetric (-1) 600 "Ncommaaccent" (8, -250, 610, 562) []
    , AfmCharMetric (-1) 600 "lacute" (77, 0, 523, 801) []
    , AfmCharMetric (-1) 600 "agrave" (35, -15, 570, 661) []
    , AfmCharMetric (-1) 600 "Tcommaaccent" (21, -250, 579, 562) []
    , AfmCharMetric (-1) 600 "Cacute" (22, -18, 560, 784) []
    , AfmCharMetric (-1) 600 "atilde" (35, -15, 570, 636) []
    , AfmCharMetric (-1) 600 "Edotaccent" (25, 0, 560, 761) []
    , AfmCharMetric (-1) 600 "scaron" (68, -17, 535, 667) []
    , AfmCharMetric (-1) 600 "scedilla" (68, -206, 535, 459) []
    , AfmCharMetric (-1) 600 "iacute" (77, 0, 523, 661) []
    , AfmCharMetric (-1) 600 "lozenge" (66, 0, 534, 740) []
    , AfmCharMetric (-1) 600 "Rcaron" (24, 0, 599, 790) []
    , AfmCharMetric (-1) 600 "Gcommaaccent" (22, -250, 594, 580) []
    , AfmCharMetric (-1) 600 "ucircumflex" (-1, -15, 569, 657) []
    , AfmCharMetric (-1) 600 "acircumflex" (35, -15, 570, 657) []
    , AfmCharMetric (-1) 600 "Amacron" (-9, 0, 609, 708) []
    , AfmCharMetric (-1) 600 "rcaron" (47, 0, 580, 667) []
    , AfmCharMetric (-1) 600 "ccedilla" (40, -206, 545, 459) []
    , AfmCharMetric (-1) 600 "Zdotaccent" (62, 0, 539, 761) []
    , AfmCharMetric (-1) 600 "Thorn" (48, 0, 557, 562) []
    , AfmCharMetric (-1) 600 "Omacron" (22, -18, 578, 708) []
    , AfmCharMetric (-1) 600 "Racute" (24, 0, 599, 784) []
    , AfmCharMetric (-1) 600 "Sacute" (47, -22, 553, 784) []
    , AfmCharMetric (-1) 600 "dcaron" (20, -15, 727, 626) []
    , AfmCharMetric (-1) 600 "Umacron" (4, -18, 596, 708) []
    , AfmCharMetric (-1) 600 "uring" (-1, -15, 569, 678) []
    , AfmCharMetric (-1) 600 "threesuperior" (138, 222, 433, 616) []
    , AfmCharMetric (-1) 600 "Ograve" (22, -18, 578, 784) []
    , AfmCharMetric (-1) 600 "Agrave" (-9, 0, 609, 784) []
    , AfmCharMetric (-1) 600 "Abreve" (-9, 0, 609, 784) []
    , AfmCharMetric (-1) 600 "multiply" (81, 39, 520, 478) []
    , AfmCharMetric (-1) 600 "uacute" (-1, -15, 569, 661) []
    , AfmCharMetric (-1) 600 "Tcaron" (21, 0, 579, 790) []
    , AfmCharMetric (-1) 600 "partialdiff" (63, -38, 537, 728) []
    , AfmCharMetric (-1) 600 "ydieresis" (-4, -142, 601, 638) []
    , AfmCharMetric (-1) 600 "Nacute" (8, -12, 610, 784) []
    , AfmCharMetric (-1) 600 "icircumflex" (73, 0, 523, 657) []
    , AfmCharMetric (-1) 600 "Ecircumflex" (25, 0, 560, 780) []
    , AfmCharMetric (-1) 600 "adieresis" (35, -15, 570, 638) []
    , AfmCharMetric (-1) 600 "edieresis" (40, -15, 563, 638) []
    , AfmCharMetric (-1) 600 "cacute" (40, -15, 545, 661) []
    , AfmCharMetric (-1) 600 "nacute" (18, 0, 592, 661) []
    , AfmCharMetric (-1) 600 "umacron" (-1, -15, 569, 585) []
    , AfmCharMetric (-1) 600 "Ncaron" (8, -12, 610, 790) []
    , AfmCharMetric (-1) 600 "Iacute" (77, 0, 523, 784) []
    , AfmCharMetric (-1) 600 "plusminus" (71, 24, 529, 515) []
    , AfmCharMetric (-1) 600 "brokenbar" (255, -175, 345, 675) []
    , AfmCharMetric (-1) 600 "registered" (0, -18, 600, 580) []
    , AfmCharMetric (-1) 600 "Gbreve" (22, -18, 594, 784) []
    , AfmCharMetric (-1) 600 "Idotaccent" (77, 0, 523, 761) []
    , AfmCharMetric (-1) 600 "summation" (15, -10, 586, 706) []
    , AfmCharMetric (-1) 600 "Egrave" (25, 0, 560, 784) []
    , AfmCharMetric (-1) 600 "racute" (47, 0, 580, 661) []
    , AfmCharMetric (-1) 600 "omacron" (30, -15, 570, 585) []
    , AfmCharMetric (-1) 600 "Zacute" (62, 0, 539, 784) []
    , AfmCharMetric (-1) 600 "Zcaron" (62, 0, 539, 790) []
    , AfmCharMetric (-1) 600 "greaterequal" (26, 0, 523, 696) []
    , AfmCharMetric (-1) 600 "Eth" (30, 0, 594, 562) []
    , AfmCharMetric (-1) 600 "Ccedilla" (22, -206, 560, 580) []
    , AfmCharMetric (-1) 600 "lcommaaccent" (77, -250, 523, 626) []
    , AfmCharMetric (-1) 600 "tcaron" (47, -15, 532, 703) []
    , AfmCharMetric (-1) 600 "eogonek" (40, -199, 563, 454) []
    , AfmCharMetric (-1) 600 "Uogonek" (4, -199, 596, 562) []
    , AfmCharMetric (-1) 600 "Aacute" (-9, 0, 609, 784) []
    , AfmCharMetric (-1) 600 "Adieresis" (-9, 0, 609, 761) []
    , AfmCharMetric (-1) 600 "egrave" (40, -15, 563, 661) []
    , AfmCharMetric (-1) 600 "zacute" (81, 0, 520, 661) []
    , AfmCharMetric (-1) 600 "iogonek" (77, -199, 523, 658) []
    , AfmCharMetric (-1) 600 "Oacute" (22, -18, 578, 784) []
    , AfmCharMetric (-1) 600 "oacute" (30, -15, 570, 661) []
    , AfmCharMetric (-1) 600 "amacron" (35, -15, 570, 585) []
    , AfmCharMetric (-1) 600 "sacute" (68, -17, 535, 661) []
    , AfmCharMetric (-1) 600 "idieresis" (77, 0, 523, 618) []
    , AfmCharMetric (-1) 600 "Ocircumflex" (22, -18, 578, 780) []
    , AfmCharMetric (-1) 600 "Ugrave" (4, -18, 596, 784) []
    , AfmCharMetric (-1) 600 "Delta" (6, 0, 594, 688) []
    , AfmCharMetric (-1) 600 "thorn" (-14, -142, 570, 626) []
    , AfmCharMetric (-1) 600 "twosuperior" (143, 230, 436, 616) []
    , AfmCharMetric (-1) 600 "Odieresis" (22, -18, 578, 761) []
    , AfmCharMetric (-1) 600 "mu" (-1, -142, 569, 439) []
    , AfmCharMetric (-1) 600 "igrave" (77, 0, 523, 661) []
    , AfmCharMetric (-1) 600 "ohungarumlaut" (30, -15, 668, 661) []
    , AfmCharMetric (-1) 600 "Eogonek" (25, -199, 576, 562) []
    , AfmCharMetric (-1) 600 "dcroat" (20, -15, 591, 626) []
    , AfmCharMetric (-1) 600 "threequarters" (-47, -60, 648, 661) []
    , AfmCharMetric (-1) 600 "Scedilla" (47, -206, 553, 582) []
    , AfmCharMetric (-1) 600 "lcaron" (77, 0, 597, 626) []
    , AfmCharMetric (-1) 600 "Kcommaaccent" (21, -250, 599, 562) []
    , AfmCharMetric (-1) 600 "Lacute" (39, 0, 578, 784) []
    , AfmCharMetric (-1) 600 "trademark" (-9, 230, 749, 562) []
    , AfmCharMetric (-1) 600 "edotaccent" (40, -15, 563, 638) []
    , AfmCharMetric (-1) 600 "Igrave" (77, 0, 523, 784) []
    , AfmCharMetric (-1) 600 "Imacron" (77, 0, 523, 708) []
    , AfmCharMetric (-1) 600 "Lcaron" (39, 0, 637, 562) []
    , AfmCharMetric (-1) 600 "onehalf" (-47, -60, 648, 661) []
    , AfmCharMetric (-1) 600 "lessequal" (26, 0, 523, 696) []
    , AfmCharMetric (-1) 600 "ocircumflex" (30, -15, 570, 657) []
    , AfmCharMetric (-1) 600 "ntilde" (18, 0, 592, 636) []
    , AfmCharMetric (-1) 600 "Uhungarumlaut" (4, -18, 638, 784) []
    , AfmCharMetric (-1) 600 "Eacute" (25, 0, 560, 784) []
    , AfmCharMetric (-1) 600 "emacron" (40, -15, 563, 585) []
    , AfmCharMetric (-1) 600 "gbreve" (30, -146, 580, 661) []
    , AfmCharMetric (-1) 600 "onequarter" (-56, -60, 656, 661) []
    , AfmCharMetric (-1) 600 "Scaron" (47, -22, 553, 790) []
    , AfmCharMetric (-1) 600 "Scommaaccent" (47, -250, 553, 582) []
    , AfmCharMetric (-1) 600 "Ohungarumlaut" (22, -18, 628, 784) []
    , AfmCharMetric (-1) 600 "degree" (86, 243, 474, 616) []
    , AfmCharMetric (-1) 600 "ograve" (30, -15, 570, 661) []
    , AfmCharMetric (-1) 600 "Ccaron" (22, -18, 560, 790) []
    , AfmCharMetric (-1) 600 "ugrave" (-1, -15, 569, 661) []
    , AfmCharMetric (-1) 600 "radical" (-19, -104, 473, 778) []
    , AfmCharMetric (-1) 600 "Dcaron" (30, 0, 594, 790) []
    , AfmCharMetric (-1) 600 "rcommaaccent" (47, -250, 580, 454) []
    , AfmCharMetric (-1) 600 "Ntilde" (8, -12, 610, 759) []
    , AfmCharMetric (-1) 600 "otilde" (30, -15, 570, 636) []
    , AfmCharMetric (-1) 600 "Rcommaaccent" (24, -250, 599, 562) []
    , AfmCharMetric (-1) 600 "Lcommaaccent" (39, -250, 578, 562) []
    , AfmCharMetric (-1) 600 "Atilde" (-9, 0, 609, 759) []
    , AfmCharMetric (-1) 600 "Aogonek" (-9, -199, 625, 562) []
    , AfmCharMetric (-1) 600 "Aring" (-9, 0, 609, 801) []
    , AfmCharMetric (-1) 600 "Otilde" (22, -18, 578, 759) []
    , AfmCharMetric (-1) 600 "zdotaccent" (81, 0, 520, 638) []
    , AfmCharMetric (-1) 600 "Ecaron" (25, 0, 560, 790) []
    , AfmCharMetric (-1) 600 "Iogonek" (77, -199, 523, 562) []
    , AfmCharMetric (-1) 600 "kcommaaccent" (20, -250, 585, 626) []
    , AfmCharMetric (-1) 600 "minus" (71, 203, 529, 313) []
    , AfmCharMetric (-1) 600 "Icircumflex" (77, 0, 523, 780) []
    , AfmCharMetric (-1) 600 "ncaron" (18, 0, 592, 667) []
    , AfmCharMetric (-1) 600 "tcommaaccent" (47, -250, 532, 562) []
    , AfmCharMetric (-1) 600 "logicalnot" (71, 103, 529, 413) []
    , AfmCharMetric (-1) 600 "odieresis" (30, -15, 570, 638) []
    , AfmCharMetric (-1) 600 "udieresis" (-1, -15, 569, 638) []
    , AfmCharMetric (-1) 600 "notequal" (12, -47, 537, 563) []
    , AfmCharMetric (-1) 600 "gcommaaccent" (30, -146, 580, 714) []
    , AfmCharMetric (-1) 600 "eth" (58, -27, 543, 626) []
    , AfmCharMetric (-1) 600 "zcaron" (81, 0, 520, 667) []
    , AfmCharMetric (-1) 600 "ncommaaccent" (18, -250, 592, 454) []
    , AfmCharMetric (-1) 600 "onesuperior" (153, 230, 447, 616) []
    , AfmCharMetric (-1) 600 "imacron" (77, 0, 523, 585) []
    , AfmCharMetric (-1) 600 "Euro" (0, 0, 0, 0) []
    ]
  , afmFontKernPairXs =
    [ 
    ]
  }
  
