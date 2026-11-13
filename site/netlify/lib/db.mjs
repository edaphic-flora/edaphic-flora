// One short-lived connection per request (Neon's pooler handles the pooling).

import pg from 'pg';

export async function withClient(connectionString, fn) {
  if (!connectionString) throw new Error('database not configured');
  const client = new pg.Client({
    connectionString,
    ssl: { rejectUnauthorized: true },
    connectionTimeoutMillis: 8000,
    statement_timeout: 15000,
    query_timeout: 20000,
  });
  await client.connect();
  try {
    return await fn(client);
  } finally {
    await client.end().catch(() => {});
  }
}

// JSON response helper shared by the functions
export function json(body, status = 200, headers = {}) {
  return new Response(body === null ? null : JSON.stringify(body), {
    status,
    headers: {
      'Content-Type': 'application/json; charset=utf-8',
      'Cache-Control': 'no-store',
      'X-Content-Type-Options': 'nosniff',
      ...headers,
    },
  });
}
