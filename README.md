# Flag operator

##### Description

The `flag operator` flags numeric or text values based on a condition.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, values to flag, if they are numeric 
`row`           | character, values to flag, if they are textual 

Input parameters|.
---|---
`type`        | whether the values to flag are `character` or `numeric`
`comparison`        | comparison to perform, any of: `equals`, `greater`, `less`, `greater_or_equal`, `less_or_equal`  (for numeric values); or  `equals`, `contains`, `is_in` (for text values)
`value`        | value to compare data to

Output relations|.
---|---
`flag`        | character, `pass` or `fail` value

##### Details

Details on the computation.

##### See Also

[replace_operator](https://github.com/tercen/replace_operator)
, [separate_operator](https://github.com/tercen/separate_operator)

