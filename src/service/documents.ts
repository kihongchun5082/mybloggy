import path from 'path'
import { readFile } from 'fs/promises';
export async function getAllDocuments() {
  const filePath = path.join(process.cwd(), 'data', 'posts')
}
