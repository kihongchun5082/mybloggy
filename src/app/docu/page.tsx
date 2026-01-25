import FilterablePosts from "@/components/FilterablePosts";
import { getAllPost } from "@/service/posts";
import { Metadata } from "next";

export const metadata: Metadata = {
  title: '책임의료조직 전기홍 블로그',
  description: '책임의료조직은 국민 건강수명 연장을 위해 반드시 우리가 해 내야 할 과제입니다.'
}

export default async function Docu() {
  const posts = await getAllPost();
  // console.log('posts;', posts)
  const postCategory = posts.map((post) => post.category)
  // console.log('postCategory;', postCategory)
  const categories = [...new Set(postCategory)]
  // console.log('categories;', categories)
  // const newSet = new Set(posts.map((post) => post.category))
  return <FilterablePosts posts={posts} categories={categories} />
}

// tsconfig.json에  "downlevelIteration": true, 를 추가해야 타입스크립트 이터레이션 Set() 가능9   함