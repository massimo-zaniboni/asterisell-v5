# Problem 

Open a problem, for viewing more details, because the list format them in wrong way.

Problem list is more a status board than a log, because only active problems will be showed.
After every change of customer data, a complete rerating process will be executed automatically,
and only current problems will be in the list. Use the ``refresh`` button.

Few problems marked as ``Tested every time: False``:

* will be still present also if resolved, so they can create garbage in the error list, so you can use the ``Delete all problems`` button for removing them
* for few cases of problems with transient state, that can not be tested every time the job processor is running, using the ``Delete all problems`` button is dangerous because a real problem signalation can be lost. This situation is signaled in the problem description.

