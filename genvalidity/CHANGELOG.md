Unreleased changes:
* -0 is now a valid value for Double and Float.
* `genUnchecked :: Gen Double` now also generates invalid values.
* arbPartition now shuffles the partitions, which means that genListOf produces lists of elements with shuffled sizes. This also fixes the same problem with `instance GenUnchecked a => GenUnchecked [a]`.
