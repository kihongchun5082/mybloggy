import { getAllPost, getFeaturedPost } from "../service/posts";
import DocuGrid from "./DocuGrid";

// import {compile} from '@mdx-js/mdx'
// import rehypeKatex from 'rehype-katex' // Render math with KaTeX.
// import remarkFrontmatter from 'remark-frontmatter' // YAML and such.
// import remarkGfm from 'remark-gfm' // Tables, footnotes, strikethrough, task lists, literal URLs.
// import remarkMath from 'remark-math' // Support math like `$so$`.

export default async function MyThoughts() {
  const posts = await getFeaturedPost()
  return (
    <section className=" my-4">
      <h2 className=" text-2xl font-bold m-2">나는 이렇게 생각합니다.</h2>
      <DocuGrid posts={posts}/>

{/* 
const file = '# hi'

// One plugin:
await compile(file, {remarkPlugins: [remarkGfm]})

// A plugin with options:
await compile(file, {remarkPlugins: [[remarkFrontmatter, 'toml']]})

// Two plugins:
await compile(file, {remarkPlugins: [remarkGfm, remarkFrontmatter]})

// Two plugins, first w/ options:
await compile(file, {remarkPlugins: [[remarkGfm, {singleTilde: false}], remarkFrontmatter]})

// remark and rehype plugins:
await compile(file, {rehypePlugins: [rehypeKatex], remarkPlugins: [remarkMath]})

// remark and rehype plugins, last w/ options:
await compile(file, {
  // A plugin with options:
  rehypePlugins: [[rehypeKatex, {strict: true, throwOnError: true}]],
  remarkPlugins: [remarkMath]
})

 */}
    </section>
  );
}

