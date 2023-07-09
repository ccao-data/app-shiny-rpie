# RPIE Code Retrieval App

This repository contains a [Shiny app](https://datascience.cookcountyassessor.com/shiny/app/rpie) that users within the CCAO can use to retrieve RPIE codes for taxpayers.

There are currently two ways to go about this:

* Enter a single PIN and RPIE reporting year (the current year), and the app will display the PIN, its RPIE code, and the address it was mailed to.

* Upload a list of PINs contained in a `.csv` or `.xlsx` (you should still choose a year); the only real criteria for this upload is that there is only one PIN per row and the column containing PINs is named "PIN".  The PIN data itself does not need to be clean (it can contain spaces, hyphens, commas, etc.), PINs can be 10 or 14 digits, and extra columns included in the upload will be preserved. <br><br> The app will generate a table with each PIN's RPIE code appended onto the right-side of the uploaded data, and allow users to download all the codes in bulk as a `.csv` or `.xlsx` file (or simply copy them).

That's it!
