# TextQL Take-home Challenge

A web interface for a SQL parser, allowing users to execute queries against JSON data and view query history.

## Prereqs

- GHC and Cabal (Haskell build tools)
- Node.js and npm
- SQLite

## Building and Running

### Backend

1. Navigate to the backend directory:
   ```bash
   cd backend
   ```

2. Build the backend:
   ```bash
   cabal build
   ```

3. Run the backend server:
   ```bash
   cabal run textql-server -- data/data.json
   ```
   The server will start on port 3000.

   Sample data (sample.json) is included in my submission.

### Frontend

1. Navigate to the frontend directory:
   ```bash
   cd frontend
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Start the development server:
   ```bash
   npm run dev
   ```

4. Open your browser to `http://localhost:5173`

## Usage

1. Enter a SQL query in the text box
2. Click "Execute Query" to run the query
3. View the results in the table below
4. Query history is displayed at the bottom of the page

## Example Queries

```sql
SELECT * FROM table WHERE state = 'California' LIMIT 10;
SELECT name, population FROM table WHERE population > 1000000;
```

## Error Handling

- Invalid SQL queries will display an error message
- The application will not crash on invalid input
- Query history is preserved even for failed queries 