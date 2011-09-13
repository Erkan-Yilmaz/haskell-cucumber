Feature: a cucumber lookalike for haskell
  Scenario: A step is called with the matched groups
    Given a step 'value' matches 'value' 

  Scenario: tables
    Given step gets the argument:
    | foo |
    | bar |


  Scenario-outline: table outline
    Given a table <values>
    Examples:
    | values |
    | first  |

  Scenario-outline: expand var in pystring
    Given a pystring:
    """
    <values>
    """
    Examples:
    | values |
    | Foobar |
  
  Scenario-outline: expand var in table
    Given expand table values:
    | values   |
    | <expand> |
    Examples:
    | expand |
    | foo    |
  
    
    
