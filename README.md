## From shiny to googlesheet (and googledrive!) demo app

### Welcome 👋

This app is a simple showcase of how Shiny can work with googlesheet to handle tracker data. It's a demo, so it's not packed with features, but it's got enough to give you a taste of what's possible ✨. This app:

- Retrieves its primary data directly from a Google Sheet.
- Enables users to update the Google Sheet directly within the application.
- Includes a feature that allows users to upload images to Google Drive and subsequently update the corresponding links in the Google Sheet.

### How to Run 🏃‍♀️
1. **Restore dependencies:**
   This app is built using `rhino` which relies on `renv` for managing dependencies. Start by running `renv::restore()` to install all necessary packages.
2. **Set up environment variable:**
   You'll need to create a `.Renviron` file in your project directory. Add these variables to it:
   - `GOOGLE_MAIL`: your google mail
   - `GOOGLE_SHEET_URL`: Paste [this](https://docs.google.com/spreadsheets/d/1DTgvB2-tZpb37maYWc18sDQ2xQ_jGVlefYGCU08mvvc/edit?usp=sharing) googlesheet url.
3. Run with `rhino::app()`

### Using the app 🚜

The app has two main pages:

#### Tabel SPR

- This page displays a table with customer data and their mortgage application statuses. To update a status, simply click on the row you want to modify.
  
#### Tracker

- The tracker page features three distinct cards:
  - **Info Card**: Summarizes customer details based on the lot number selected.
  - **Bukti Pembayaran Bank**: This card focuses on tracking the disbursements from the bank.
  - **Pembayaran Kontraktor**: Keeps track of construction progress and contractor payments.<br><br>
- **Bukti Pembayaran Bank** and **Pembayaran Kontraktor** cards share the following functionalities:
  - Display a list of buttons indicating dates and amounts of transactions. Clicking on a button opens a new page that links to a Google Drive image showing the payment proof.
  - The `+ Tambah baru` button allows users to add new transaction proofs. Users can enter transaction details and upload an image of the payment proof, which then updates the relevant links in the Google Sheet.

### Your thoughts? 🐱

If you have suggestions or improvements, I’m open to hearing them. Feel free to fork the repo or send over your ideas 🌈 ✨!
