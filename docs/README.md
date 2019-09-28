# scm-1-delay @ d8euAI8sMs

## TODO и качество кода

Определенно, в плане качества кода на Haskell есть к чему стремиться.

* Во-первых, работа со случайными числами ведется отвратительно. Много где переиспользуется один и тот же генератор.
* ~~Во-вторых, так же визуально отвратительно идет работа в монаде `ST`.~~ Она заменена на монаду `State`, теперь это выглядит намного лучше.
* Точка входа оформлена не самым лучшим образом. В т.ч. из-за необходимости таскать за собой генераторы случайных чисел.
* В некоторых местах код дублируется (как логически, так и физически).
* Код в точке входа просто некрасив. Смешение `let` и `<-` без какого-либо порядка. Из-за name clashes поля некоторых типов данных названы ужасно. Использование самих типов выглядит неорганично.
* Производительность можно улучшить.
* Можно отказаться от списков `[]` совсем в пользу `Data.Vector` (в т.ч. `Storable`).