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
`comparison`        | comparison to perform, any of: `equals`, `greater`, `less`, `greater_or_equal`, `less_or_equal`, `top`, `bottom`  (for numeric values); or  `equals`, `contains`, `is_in` (for text values)
`value`        | value to compare data to

Output relations|.
---|---
`flag`        | character, `pass` or `fail` value

##### Details

For `top` and `bottom` flagging:

* if `value` is between 0 and 1, the `top` or `bottom`  `value * 100` % values are flagged as `pass`

* if `value` is an integer greater or equal to 1, the `top` or `bottom` `n` values are flagged as `pass` (with `n = value`)  

Examples:

* If `comparison` is `top` and `value` = 0.5, a `pass` flag will be assigned to the top 50 % values, per cell.

* If `comparison` is `bottom` and `value` = 10, a `pass` flag will be assigned to the 100 lowest values, per cell.

##### See Also

[replace_operator](https://github.com/tercen/replace_operator)
, [separate_operator](https://github.com/tercen/separate_operator)

