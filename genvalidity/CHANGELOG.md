Unreleased changes:
* -0 is now a valid value for Double and Float.
* arbPartition now shuffles the partitions, which means that genListOf produces lists of elements with shuffled sizes. This also fixes the same problem with `instance GenUnchecked a => GenUnchecked [a]`.
