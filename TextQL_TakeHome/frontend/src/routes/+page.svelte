<script>
  import { onMount } from 'svelte';
  
  let query = '';
  let result = null;
  let error = null;
  let history = [];
  let loading = false;

  async function submitQuery() {
    loading = true;
    error = null;
    result = null;
    
    try {
      const response = await fetch('http://localhost:3000/query', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(query)
      });
      
      const data = await response.json();
      
      if (data.error) {
        error = data.error;
      } else {
        result = data.result;
      }
      
      // Refresh history
      await loadHistory();
    } catch (e) {
      error = 'Failed to execute query: ' + e.message;
    } finally {
      loading = false;
    }
  }

  async function loadHistory() {
    try {
      const response = await fetch('http://localhost:3000/history');
      history = await response.json();
    } catch (e) {
      console.error('Failed to load history:', e);
    }
  }

  onMount(loadHistory);
</script>

<main>
  <h1>TextQL Query Interface</h1>
  
  <div class="query-input">
    <textarea
      bind:value={query}
      placeholder="Enter your SQL query here..."
      rows="4"
    ></textarea>
    <button on:click={submitQuery} disabled={loading}>
      {loading ? 'Executing...' : 'Execute Query'}
    </button>
  </div>

  {#if error}
    <div class="error">
      <h2>Error</h2>
      <pre>{error}</pre>
    </div>
  {/if}

  {#if result}
    <div class="result">
      <h2>Result</h2>
      <table>
        <thead>
          <tr>
            {#if result.length > 0}
              {#each Object.keys(result[0]) as key}
                <th>{key}</th>
              {/each}
            {/if}
          </tr>
        </thead>
        <tbody>
          {#each result as row}
            <tr>
              {#each Object.entries(row) as [key, value]}
                <td>{value}</td>
              {/each}
            </tr>
          {/each}
        </tbody>
      </table>
    </div>
  {/if}

  <div class="history">
    <h2>Query History</h2>
    {#each history as item}
      <div class="history-item">
        <div class="query">{item.query}</div>
        <div class="timestamp">{item.timestamp}</div>
        <div class="result">{item.result}</div>
      </div>
    {/each}
  </div>
</main>

<style>
  main {
    max-width: 800px;
    margin: 0 auto;
    padding: 2rem;
  }

  .query-input {
    margin-bottom: 2rem;
  }

  textarea {
    width: 100%;
    margin-bottom: 1rem;
    padding: 0.5rem;
    font-family: monospace;
  }

  button {
    padding: 0.5rem 1rem;
    background: #4CAF50;
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
  }

  button:disabled {
    background: #cccccc;
  }

  .error {
    color: red;
    margin: 1rem 0;
    padding: 1rem;
    background: #ffebee;
    border-radius: 4px;
  }

  .result {
    margin: 1rem 0;
  }

  table {
    width: 100%;
    border-collapse: collapse;
  }

  th, td {
    border: 1px solid #ddd;
    padding: 8px;
    text-align: left;
  }

  th {
    background-color: #f2f2f2;
  }

  .history {
    margin-top: 2rem;
  }

  .history-item {
    border: 1px solid #ddd;
    margin-bottom: 1rem;
    padding: 1rem;
  }

  .history-item .query {
    font-family: monospace;
    margin-bottom: 0.5rem;
  }

  .history-item .timestamp {
    color: #666;
    font-size: 0.8rem;
  }
</style>
