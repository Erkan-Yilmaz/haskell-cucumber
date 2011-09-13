Feature: a cucumber lookalike for haskell
  Scenario: first scenario
    Given a step

  Scenario: with table argument
    Given the table:
    | foo | fuu |
    | boo | buu |

  Scenario-outline: outline
    Given a table <value>
    Examples:
    | values |
    | first  |
    | second |

  Scenario-outline: expand var in pystring
    Given a pystring:
    """
    <values>
    """
    Examples:
    | values |
    | Foobar |
