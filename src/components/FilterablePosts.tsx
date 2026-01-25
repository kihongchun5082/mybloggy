'use client'

import { Post } from "@/service/posts";
import DocuGrid from "./DocuGrid";
import Categories from "./Categories";
import { useState } from "react";

type Props = {
  posts: Post[],
  categories: string[]
}
const ALL_POSTS = '모든 포스트'
export default function FilterablePosts({ posts, categories }: Props) {
  const [selected, setSelected] = useState(ALL_POSTS)
  const filtered =
    selected === ALL_POSTS
      ? posts
      : posts.filter((post) => post.category === selected);
  return (
    <section className=" flex m-4">
      <div className=" flex-auto w-72">
        <DocuGrid posts={filtered} />
      </div>

      <div className=" flex-initial w-32">
        <Categories
          categories={[ALL_POSTS, ...categories]}
          selected={selected}
          onClick={setSelected}
          // onClick={(selected) => setSelected(selected)}/>
        />
      </div>
    </section>
  );
}

