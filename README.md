Модель биржы.

clients.txt - содержит список клиентов биржи с указанием их исходных балансов по торгующимся ценным бумагам и доллару

Файл списка клиетов имеет следующие поля:
 * Имя клиента
 * Баланс клиента по долларам 
 * Баланс клиента по ценной бумаге "A" в штуках
 * Баланс по ценной бумаге "B"
 * Баланс по ценной бумаге "C"
 * Баланс по ценной бумаге "D"



orders.txt - список заявок от клиентов в хронологическом порядке

Файл списка заявок имеет формат:
 * Имя клиента выставившего заявку
 * Символ операции: "s" - продажа или "b" - покупка.
 * Наименование ценной бумаги
 * Цена заявки (целое число за одну штуку ценной бумаги)
 * Количество продаваемых или покупаемых ценных бумаг
 
 Заявки на покупку и продажу могут отрабатывать не полностью при "сопоставлении" (т.е. с меньшим числом акций пропадает, а остаток от второй остаётся в работе) 
 
 Программа обрабатывает заявки и сохраняет результирующие балансы клиентов в файл result.txt
