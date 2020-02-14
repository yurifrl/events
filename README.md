# Events

You have just been hired by a company that provides a checkout system for many online stores. In order to make smart business decisions, the analysts at your company need detailed reports on purchases. Unfortunately, the developer who wrote this system has moved on, and you'll need to take over the codebase.

Your task is to read and understand the existing code, and then refactor it to be as clear as possible, without breaking any tests.

The original developer left some documentation, but no one at the company has any further knowledge about how the code works. Here's the documentation:

When a customer completes a purchase, an event is generated by the checkout system containing the following information:

Date: ISO 8601 instant; e.g. 2011-12-03T10:15:30Z
Amount: total amount of purchase in cents; e.g. 4285 (€42.85)
Payment method: Pay Now (credit card or bank transfer), Pay Later (invoice at the end of the month), or Slice It (pay in convenient monthly installments)
Merchant ID: unique identifier of merchant; e.g. 1bb53ed1-787b-4543-9def-ea18eef7902e
Our business people want the following reports:

Number of purchases per hour, broken down by amount bracket (less than €10, €10 - €50, €50-€100, €100-€500, more than €500)
Number of purchases per hour, broken down by amount bracket and payment method
Number of purchases, broken down by amount bracket and payment method
Number of purchases per day, broken down by merchant
Number of purchases, broken down by merchant and payment method
When a purchase is completed, the checkout system publishes an event, which is picked up by the reporting system and batched. The reporting system then invokes the aggregate function, with a list of events as its argument. An event is a JSON object:

{
  "date": "2011-12-03T10:15:30Z",
  "amount": 4285,
  "paymentMethod": "SLICE_IT",
  "merchantId": "1bb53ed1-787b-4543-9def-ea18eef7902e"
}
The aggregate function returns a list of aggregates for each report that needs to be generated.

For example, given the a batch containing only the event above, the function would generate an aggregate datapoint for the purchases by hour, amount bracket, and payment method report like this: 2011-12-03:10|10-50|SLICE_IT. The datapoint is then combined into an aggregate object like this:

{
  "datapoint": "2011-12-03:10|10-50|SLICE_IT",
  "events": 1
}
NOTE: The tests reflect the system's public API. We do not want to break it for now, so don't rewrite the tests. If you
really want to change the shape of the data (e. g. to make the system strictly typed), then build a translation
layer, so the tests don't break.
