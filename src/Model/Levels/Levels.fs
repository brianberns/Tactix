﻿namespace Tactix

module Levels =

    let levels =
        [|
            Exact.level1
            Exact.level2
            Exact.level3

            Intro.level1
            Intro.level2
            Intro.level3

            Dissolve.level1
            Dissolve.level2
            Dissolve.level3

            Apply.level1
            Apply.level2

            Split.level1
            Split.level2
            Split.level3
            Split.level4
            Split.level5
            Split.level6

            Negation.level1
            Negation.level2
            Negation.level3
            Negation.level4

            More.level1
            More.level2
            More.level3
            More.level4
            More.level5
            More.level6
            More.level7
            More.level8

            {
                Goals = set []
                Terms = set []
                GoalTactics = set []
                TermTactics = set []
                Instruction = $"{TacticType.exact} The End! {TacticType.exact}"
            }
        |]
