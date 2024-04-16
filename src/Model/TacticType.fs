namespace Tactix

module Text =

    let andSymbol = "🌳"
    let orSymbol  = "🎀"
    let notSymbol = "☂️"
    let implies   = "👉🏾"

// https://stackoverflow.com/questions/64929689/avoiding-the-error-where-a-module-and-a-type-definition-occur-in-two-parts-of-an
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TacticType =

    let emoji = function
        | TacticType.Intro        -> "🚀"
        | TacticType.Exact        -> "❤️"
        | TacticType.Apply        -> "👣"
        | TacticType.DissolveGoal -> "✂️"
        | TacticType.DissolveTerm -> "🪚"
        | TacticType.SplitGoal    -> "🍂"
        | TacticType.SplitTerm    -> "💥"
        | TacticType.AffirmGoal   -> "🌈"
        | TacticType.AffirmTerm   -> "🌈"

    let intro        = emoji TacticType.Intro
    let exact        = emoji TacticType.Exact
    let apply        = emoji TacticType.Apply
    let dissolveGoal = emoji TacticType.DissolveGoal
    let dissolveTerm = emoji TacticType.DissolveTerm
    let splitGoal    = emoji TacticType.SplitGoal
    let splitTerm    = emoji TacticType.SplitTerm
    let affirmGoal   = emoji TacticType.AffirmGoal
    let affirmTerm   = emoji TacticType.AffirmTerm

    let instruction = function
        | TacticType.Intro        -> $"Drag {intro} onto a ▨{Text.implies}■ goal to simplify it. The goal becomes ■ and you get a ▨ to work with."
        | TacticType.Exact        -> $"Drag {exact} onto a symbol that matches the goal"
        | TacticType.Apply        -> $"Drag {apply} onto ▨{Text.implies}■ when the goal is ■ to change the goal to ▨"
        | TacticType.DissolveGoal -> $"Drag {dissolveGoal} onto a ▨{Text.orSymbol}■ goal to simplify it into separate ▨ and ■ goals. You can then match either one."
        | TacticType.DissolveTerm -> $"Drag {dissolveTerm} onto a {Text.andSymbol} symbol to simplify it"
        | TacticType.SplitGoal    -> $"Drag {splitGoal} onto a {Text.andSymbol} goal to create separate cases"
        | TacticType.SplitTerm    -> $"Drag {splitTerm} onto a {Text.orSymbol} symbol to create separate cases"
        | TacticType.AffirmGoal   -> $"Drag {affirmGoal} onto a {Text.notSymbol} goal to remove {Text.notSymbol}"
        | TacticType.AffirmTerm   -> $"Drag {affirmTerm} onto a {Text.notSymbol} symbol to remove {Text.notSymbol}"
