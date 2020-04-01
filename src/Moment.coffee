moment = require "moment"

exports._add                 = (mult, inc, m) -> m.clone().add(mult, inc)
exports._addDurationToMoment = (m, duration)  -> m.clone().add(duration)
exports._addDurations         = (d1, d2)       -> d1.clone().add(d2)
exports._subtract            = (mult, inc, m) -> m.clone().subtract(mult, inc)
exports._duration            = (mult, inc)    -> moment.duration(mult, inc)
exports._fromUTC             = (utc)          -> moment.utc(utc)
exports._fromDate            = (date)         -> moment.utc(date)
exports._isAfter             = (m1, m2)       -> m1.isAfter(m2)
exports._isBefore            = (m1, m2)       -> m1.isBefore(m2)
exports._isSame              = (m1, m2)       -> m1.isSame(m2)
exports._now                 =                -> moment()
exports._diff                = (m1, m2)       -> moment.duration(m1.diff(m2))
exports._seconds             = (d)            -> d.seconds()
exports._format              = (m)            -> m.format()

exports.asMilliseconds = (d) -> m.asMilliseconds()
exports.fromMilliseconds = (x) -> m.duration(x)
