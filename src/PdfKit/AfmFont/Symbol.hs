{-# LANGUAGE OverloadedStrings #-}

module PdfKit.AfmFont.Symbol where

import PdfKit.AfmFont.AfmFont

afmFontSymbol :: AfmFont
afmFontSymbol =
  AfmFont
    { afmFontFontName = "Symbol",
      afmFontFullName = "Symbol",
      afmFontFamilyName = "Symbol",
      afmFontWeight = "Medium",
      afmFontItalicAngle = 0,
      afmFontIsFixedPitch = False,
      afmFontCharacterSet = "Special",
      afmFontFontBBox = (-180, -293, 1090, 1010),
      afmFontUnderlinePosition = -100,
      afmFontUnderlineThickness = 50,
      afmFontVersion = "001.008",
      afmFontEncodingScheme = "FontSpecific",
      afmFontCapHeight = Nothing,
      afmFontXHeight = Nothing,
      afmFontAscender = Nothing,
      afmFontDescender = Nothing,
      afmFontStdHW = 92,
      afmFontStdVW = 85,
      afmFontCharMetrics =
        [ AfmCharMetric 32 250 "space" (0, 0, 0, 0) [],
          AfmCharMetric 33 333 "exclam" (128, -17, 240, 672) [],
          AfmCharMetric 34 713 "universal" (31, 0, 681, 705) [],
          AfmCharMetric 35 500 "numbersign" (20, -16, 481, 673) [],
          AfmCharMetric 36 549 "existential" (25, 0, 478, 707) [],
          AfmCharMetric 37 833 "percent" (63, -36, 771, 655) [],
          AfmCharMetric 38 778 "ampersand" (41, -18, 750, 661) [],
          AfmCharMetric 39 439 "suchthat" (48, -17, 414, 500) [],
          AfmCharMetric 40 333 "parenleft" (53, -191, 300, 673) [],
          AfmCharMetric 41 333 "parenright" (30, -191, 277, 673) [],
          AfmCharMetric 42 500 "asteriskmath" (65, 134, 427, 551) [],
          AfmCharMetric 43 549 "plus" (10, 0, 539, 533) [],
          AfmCharMetric 44 250 "comma" (56, -152, 194, 104) [],
          AfmCharMetric 45 549 "minus" (11, 233, 535, 288) [],
          AfmCharMetric 46 250 "period" (69, -17, 181, 95) [],
          AfmCharMetric 47 278 "slash" (0, -18, 254, 646) [],
          AfmCharMetric 48 500 "zero" (24, -14, 476, 685) [],
          AfmCharMetric 49 500 "one" (117, 0, 390, 673) [],
          AfmCharMetric 50 500 "two" (25, 0, 475, 685) [],
          AfmCharMetric 51 500 "three" (43, -14, 435, 685) [],
          AfmCharMetric 52 500 "four" (15, 0, 469, 685) [],
          AfmCharMetric 53 500 "five" (32, -14, 445, 690) [],
          AfmCharMetric 54 500 "six" (34, -14, 468, 685) [],
          AfmCharMetric 55 500 "seven" (24, -16, 448, 673) [],
          AfmCharMetric 56 500 "eight" (56, -14, 445, 685) [],
          AfmCharMetric 57 500 "nine" (30, -18, 459, 685) [],
          AfmCharMetric 58 278 "colon" (81, -17, 193, 460) [],
          AfmCharMetric 59 278 "semicolon" (83, -152, 221, 460) [],
          AfmCharMetric 60 549 "less" (26, 0, 523, 522) [],
          AfmCharMetric 61 549 "equal" (11, 141, 537, 390) [],
          AfmCharMetric 62 549 "greater" (26, 0, 523, 522) [],
          AfmCharMetric 63 444 "question" (70, -17, 412, 686) [],
          AfmCharMetric 64 549 "congruent" (11, 0, 537, 475) [],
          AfmCharMetric 65 722 "Alpha" (4, 0, 684, 673) [],
          AfmCharMetric 66 667 "Beta" (29, 0, 592, 673) [],
          AfmCharMetric 67 722 "Chi" (-9, 0, 704, 673) [],
          AfmCharMetric 68 612 "Delta" (6, 0, 608, 688) [],
          AfmCharMetric 69 611 "Epsilon" (32, 0, 617, 673) [],
          AfmCharMetric 70 763 "Phi" (26, 0, 741, 673) [],
          AfmCharMetric 71 603 "Gamma" (24, 0, 609, 673) [],
          AfmCharMetric 72 722 "Eta" (39, 0, 729, 673) [],
          AfmCharMetric 73 333 "Iota" (32, 0, 316, 673) [],
          AfmCharMetric 74 631 "theta1" (18, -18, 623, 689) [],
          AfmCharMetric 75 722 "Kappa" (35, 0, 722, 673) [],
          AfmCharMetric 76 686 "Lambda" (6, 0, 680, 688) [],
          AfmCharMetric 77 889 "Mu" (28, 0, 887, 673) [],
          AfmCharMetric 78 722 "Nu" (29, -8, 720, 673) [],
          AfmCharMetric 79 722 "Omicron" (41, -17, 715, 685) [],
          AfmCharMetric 80 768 "Pi" (25, 0, 745, 673) [],
          AfmCharMetric 81 741 "Theta" (41, -17, 715, 685) [],
          AfmCharMetric 82 556 "Rho" (28, 0, 563, 673) [],
          AfmCharMetric 83 592 "Sigma" (5, 0, 589, 673) [],
          AfmCharMetric 84 611 "Tau" (33, 0, 607, 673) [],
          AfmCharMetric 85 690 "Upsilon" (-8, 0, 694, 673) [],
          AfmCharMetric 86 439 "sigma1" (40, -233, 436, 500) [],
          AfmCharMetric 87 768 "Omega" (34, 0, 736, 688) [],
          AfmCharMetric 88 645 "Xi" (40, 0, 599, 673) [],
          AfmCharMetric 89 795 "Psi" (15, 0, 781, 684) [],
          AfmCharMetric 90 611 "Zeta" (44, 0, 636, 673) [],
          AfmCharMetric 91 333 "bracketleft" (86, -155, 299, 674) [],
          AfmCharMetric 92 863 "therefore" (163, 0, 701, 487) [],
          AfmCharMetric 93 333 "bracketright" (33, -155, 246, 674) [],
          AfmCharMetric 94 658 "perpendicular" (15, 0, 652, 674) [],
          AfmCharMetric 95 500 "underscore" (-2, -125, 502, -75) [],
          AfmCharMetric 96 500 "radicalex" (480, 881, 1090, 917) [],
          AfmCharMetric 97 631 "alpha" (41, -18, 622, 500) [],
          AfmCharMetric 98 549 "beta" (61, -223, 515, 741) [],
          AfmCharMetric 99 549 "chi" (12, -231, 522, 499) [],
          AfmCharMetric 100 494 "delta" (40, -19, 481, 740) [],
          AfmCharMetric 101 439 "epsilon" (22, -19, 427, 502) [],
          AfmCharMetric 102 521 "phi" (28, -224, 492, 673) [],
          AfmCharMetric 103 411 "gamma" (5, -225, 484, 499) [],
          AfmCharMetric 104 603 "eta" (0, -202, 527, 514) [],
          AfmCharMetric 105 329 "iota" (0, -17, 301, 503) [],
          AfmCharMetric 106 603 "phi1" (36, -224, 587, 499) [],
          AfmCharMetric 107 549 "kappa" (33, 0, 558, 501) [],
          AfmCharMetric 108 549 "lambda" (24, -17, 548, 739) [],
          AfmCharMetric 109 576 "mu" (33, -223, 567, 500) [],
          AfmCharMetric 110 521 "nu" (-9, -16, 475, 507) [],
          AfmCharMetric 111 549 "omicron" (35, -19, 501, 499) [],
          AfmCharMetric 112 549 "pi" (10, -19, 530, 487) [],
          AfmCharMetric 113 521 "theta" (43, -17, 485, 690) [],
          AfmCharMetric 114 549 "rho" (50, -230, 490, 499) [],
          AfmCharMetric 115 603 "sigma" (30, -21, 588, 500) [],
          AfmCharMetric 116 439 "tau" (10, -19, 418, 500) [],
          AfmCharMetric 117 576 "upsilon" (7, -18, 535, 507) [],
          AfmCharMetric 118 713 "omega1" (12, -18, 671, 583) [],
          AfmCharMetric 119 686 "omega" (42, -17, 684, 500) [],
          AfmCharMetric 120 493 "xi" (27, -224, 469, 766) [],
          AfmCharMetric 121 686 "psi" (12, -228, 701, 500) [],
          AfmCharMetric 122 494 "zeta" (60, -225, 467, 756) [],
          AfmCharMetric 123 480 "braceleft" (58, -183, 397, 673) [],
          AfmCharMetric 124 200 "bar" (65, -293, 135, 707) [],
          AfmCharMetric 125 480 "braceright" (79, -183, 418, 673) [],
          AfmCharMetric 126 549 "similar" (17, 203, 529, 307) [],
          AfmCharMetric 160 750 "Euro" (20, -12, 714, 685) [],
          AfmCharMetric 161 620 "Upsilon1" (-2, 0, 610, 685) [],
          AfmCharMetric 162 247 "minute" (27, 459, 228, 735) [],
          AfmCharMetric 163 549 "lessequal" (29, 0, 526, 639) [],
          AfmCharMetric 164 167 "fraction" (-180, -12, 340, 677) [],
          AfmCharMetric 165 713 "infinity" (26, 124, 688, 404) [],
          AfmCharMetric 166 500 "florin" (2, -193, 494, 686) [],
          AfmCharMetric 167 753 "club" (86, -26, 660, 533) [],
          AfmCharMetric 168 753 "diamond" (142, -36, 600, 550) [],
          AfmCharMetric 169 753 "heart" (117, -33, 631, 532) [],
          AfmCharMetric 170 753 "spade" (113, -36, 629, 548) [],
          AfmCharMetric 171 1042 "arrowboth" (24, -15, 1024, 511) [],
          AfmCharMetric 172 987 "arrowleft" (32, -15, 942, 511) [],
          AfmCharMetric 173 603 "arrowup" (45, 0, 571, 910) [],
          AfmCharMetric 174 987 "arrowright" (49, -15, 959, 511) [],
          AfmCharMetric 175 603 "arrowdown" (45, -22, 571, 888) [],
          AfmCharMetric 176 400 "degree" (50, 385, 350, 685) [],
          AfmCharMetric 177 549 "plusminus" (10, 0, 539, 645) [],
          AfmCharMetric 178 411 "second" (20, 459, 413, 737) [],
          AfmCharMetric 179 549 "greaterequal" (29, 0, 526, 639) [],
          AfmCharMetric 180 549 "multiply" (17, 8, 533, 524) [],
          AfmCharMetric 181 713 "proportional" (27, 123, 639, 404) [],
          AfmCharMetric 182 494 "partialdiff" (26, -20, 462, 746) [],
          AfmCharMetric 183 460 "bullet" (50, 113, 410, 473) [],
          AfmCharMetric 184 549 "divide" (10, 71, 536, 456) [],
          AfmCharMetric 185 549 "notequal" (15, -25, 540, 549) [],
          AfmCharMetric 186 549 "equivalence" (14, 82, 538, 443) [],
          AfmCharMetric 187 549 "approxequal" (14, 135, 527, 394) [],
          AfmCharMetric 188 1000 "ellipsis" (111, -17, 889, 95) [],
          AfmCharMetric 189 603 "arrowvertex" (280, -120, 336, 1010) [],
          AfmCharMetric 190 1000 "arrowhorizex" (-60, 220, 1050, 276) [],
          AfmCharMetric 191 658 "carriagereturn" (15, -16, 602, 629) [],
          AfmCharMetric 192 823 "aleph" (175, -18, 661, 658) [],
          AfmCharMetric 193 686 "Ifraktur" (10, -53, 578, 740) [],
          AfmCharMetric 194 795 "Rfraktur" (26, -15, 759, 734) [],
          AfmCharMetric 195 987 "weierstrass" (159, -211, 870, 573) [],
          AfmCharMetric 196 768 "circlemultiply" (43, -17, 733, 673) [],
          AfmCharMetric 197 768 "circleplus" (43, -15, 733, 675) [],
          AfmCharMetric 198 823 "emptyset" (39, -24, 781, 719) [],
          AfmCharMetric 199 768 "intersection" (40, 0, 732, 509) [],
          AfmCharMetric 200 768 "union" (40, -17, 732, 492) [],
          AfmCharMetric 201 713 "propersuperset" (20, 0, 673, 470) [],
          AfmCharMetric 202 713 "reflexsuperset" (20, -125, 673, 470) [],
          AfmCharMetric 203 713 "notsubset" (36, -70, 690, 540) [],
          AfmCharMetric 204 713 "propersubset" (37, 0, 690, 470) [],
          AfmCharMetric 205 713 "reflexsubset" (37, -125, 690, 470) [],
          AfmCharMetric 206 713 "element" (45, 0, 505, 468) [],
          AfmCharMetric 207 713 "notelement" (45, -58, 505, 555) [],
          AfmCharMetric 208 768 "angle" (26, 0, 738, 673) [],
          AfmCharMetric 209 713 "gradient" (36, -19, 681, 718) [],
          AfmCharMetric 210 790 "registerserif" (50, -17, 740, 673) [],
          AfmCharMetric 211 790 "copyrightserif" (51, -15, 741, 675) [],
          AfmCharMetric 212 890 "trademarkserif" (18, 293, 855, 673) [],
          AfmCharMetric 213 823 "product" (25, -101, 803, 751) [],
          AfmCharMetric 214 549 "radical" (10, -38, 515, 917) [],
          AfmCharMetric 215 250 "dotmath" (69, 210, 169, 310) [],
          AfmCharMetric 216 713 "logicalnot" (15, 0, 680, 288) [],
          AfmCharMetric 217 603 "logicaland" (23, 0, 583, 454) [],
          AfmCharMetric 218 603 "logicalor" (30, 0, 578, 477) [],
          AfmCharMetric 219 1042 "arrowdblboth" (27, -20, 1023, 510) [],
          AfmCharMetric 220 987 "arrowdblleft" (30, -15, 939, 513) [],
          AfmCharMetric 221 603 "arrowdblup" (39, 2, 567, 911) [],
          AfmCharMetric 222 987 "arrowdblright" (45, -20, 954, 508) [],
          AfmCharMetric 223 603 "arrowdbldown" (44, -19, 572, 890) [],
          AfmCharMetric 224 494 "lozenge" (18, 0, 466, 745) [],
          AfmCharMetric 225 329 "angleleft" (25, -198, 306, 746) [],
          AfmCharMetric 226 790 "registersans" (50, -20, 740, 670) [],
          AfmCharMetric 227 790 "copyrightsans" (49, -15, 739, 675) [],
          AfmCharMetric 228 786 "trademarksans" (5, 293, 725, 673) [],
          AfmCharMetric 229 713 "summation" (14, -108, 695, 752) [],
          AfmCharMetric 230 384 "parenlefttp" (24, -293, 436, 926) [],
          AfmCharMetric 231 384 "parenleftex" (24, -85, 108, 925) [],
          AfmCharMetric 232 384 "parenleftbt" (24, -293, 436, 926) [],
          AfmCharMetric 233 384 "bracketlefttp" (0, -80, 349, 926) [],
          AfmCharMetric 234 384 "bracketleftex" (0, -79, 77, 925) [],
          AfmCharMetric 235 384 "bracketleftbt" (0, -80, 349, 926) [],
          AfmCharMetric 236 494 "bracelefttp" (209, -85, 445, 925) [],
          AfmCharMetric 237 494 "braceleftmid" (20, -85, 284, 935) [],
          AfmCharMetric 238 494 "braceleftbt" (209, -75, 445, 935) [],
          AfmCharMetric 239 494 "braceex" (209, -85, 284, 935) [],
          AfmCharMetric 241 329 "angleright" (21, -198, 302, 746) [],
          AfmCharMetric 242 274 "integral" (2, -107, 291, 916) [],
          AfmCharMetric 243 686 "integraltp" (308, -88, 675, 920) [],
          AfmCharMetric 244 686 "integralex" (308, -88, 378, 975) [],
          AfmCharMetric 245 686 "integralbt" (11, -87, 378, 921) [],
          AfmCharMetric 246 384 "parenrighttp" (54, -293, 466, 926) [],
          AfmCharMetric 247 384 "parenrightex" (382, -85, 466, 925) [],
          AfmCharMetric 248 384 "parenrightbt" (54, -293, 466, 926) [],
          AfmCharMetric 249 384 "bracketrighttp" (22, -80, 371, 926) [],
          AfmCharMetric 250 384 "bracketrightex" (294, -79, 371, 925) [],
          AfmCharMetric 251 384 "bracketrightbt" (22, -80, 371, 926) [],
          AfmCharMetric 252 494 "bracerighttp" (48, -85, 284, 925) [],
          AfmCharMetric 253 494 "bracerightmid" (209, -85, 473, 935) [],
          AfmCharMetric 254 494 "bracerightbt" (48, -75, 284, 935) [],
          AfmCharMetric (-1) 790 "apple" (56, -3, 733, 808) []
        ],
      afmFontKernPairXs = []
    }
