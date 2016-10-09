Console example of using the [Glazier FRP framework](https://github.com/louispan/glazier) with [pipes-fluid](https://github.com/louispan/pipes-fluid) for the signal network.

The Elm startApp is similar to 'startUi' in conjunction with 'exampleApp'. See [Console.hs](src/Glazier/Tutoria/Console.hs)

This example shows:
* how to setup an external signal network using pipes-fluid,
* how to feed the output from an external signal network (using pipes-fluid) into the glazier widget framework,
* how to obtain and render a frame from the glazier update tick,
* how to obtain commands from the glazier update tick,
* how to get data out of glazier model back into the signal network (see threshold TVar)