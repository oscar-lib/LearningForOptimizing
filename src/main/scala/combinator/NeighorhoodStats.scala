package combinator

/**
 * Lightweight store of information regarding the execution of a move
 *
 * @param hasImproved
 * @param noMoveFound
 * @param slope
 * @param timeNano
 */
case class RunStat(
                    hasImproved: Boolean,
                    noMoveFound: Boolean,
                    slope: Double,
                    timeNano: Long,
                  )