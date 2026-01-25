import { readFile } from "fs/promises"
import path from "path"
import { cache } from 'react';

export type Post = {
  title: string
  description: string
  date: Date
  category: string
  path: string
  fileType: string
  featured: boolean
}

export type PostData = Post & {
  content: string
  next: Post | null
  prev: Post | null
}

export async function getFeaturedPost(): Promise<Post[]> {
  return getAllPost()
    .then((posts) => posts.filter((p)=>p.featured))
  
}

export async function getNonFeaturedPost(): Promise<Post[]> {
  return getAllPost()
    .then((posts) => posts.filter(post => !post.featured))
}

/* export async function getAllPost(): Promise<Post[]> {
  console.log('getAllPosts')
  const filePath = path.join(process.cwd(), 'data', 'posts.json')
  // console.log('filePath;', filePath)
  return readFile(filePath, 'utf-8')
  .then<Post[]>(p=> JSON.parse(p))
  .then(posts=>posts.sort((a,b)=>(a.date>b.date ? -1 : 1)))
} */
export const getAllPost = cache(async () => {
  // const iCloudPath = '/Users/kihongchun/Library/Mobile Documents/com~apple~CloudDocs/myBlog'
  // const filePath = '/Users/kihongchun/Library/Mobile Documents/com~apple~CloudDocs/data/posts.json'
  // const filePath = path.join(process.cwd(), 'data', 'aco_posts.json')
  const filePath = path.join(process.cwd(), 'data', 'posts.json')
  // console.log('getAllPosts_filePath;', filePath)
  // const filePath = 'https://drive.google.com/file/d/1vPF6a42beQ3vm2YOm34VP_cY9-7dxXvh'
  // console.log('filePath;', filePath)
  // const postFilePath = path.join(process.cwd(), 'data')
  // const postFiles = {
  // }
  // const fileExt = filePath.map(p => )
  return readFile(filePath, 'utf-8')
  // .then<Post[]>(p=> JSON.parse(p))
  .then<Post[]>(JSON.parse)
  .then(posts=>posts.sort((a,b)=>(a.date>b.date ? -1 : 1)))
})

export async function getPostData(filename: string): Promise<PostData> {
  // const iCloudPath = '/Users/kihongchun/Library/Mobile Documents/com~apple~CloudDocs/myBlog'
  // const filePath = path.join(iCloudPath, 'data', 'posts', `${filename}.md`)
  const posts = await getAllPost()
  // console.log('posts;',posts)
  const post: Post | undefined = posts.find(p => p.path === filename)
  // console.log('post.fileType;', post?.fileType)
  // const filePath = path.join(process.cwd(), 'data', 'aco_posts', `${filename}.${post?.fileType}`)
  const filePath = path.join(process.cwd(), 'data', 'posts', `${filename}.${post?.fileType}`)
  // console.log('post.filePath;', filePath)
  // const filePath = path.join(process.cwd(), 'data', 'posts', `${filename}.md`)
  // const extention = path.parse(post)

  if (!post) throw new Error( `${filename}에 해당하는 파일이 없습니다.`)

  const index = posts.indexOf(post)
  const next = index > 0 ? posts[index-1] : null
  const prev = index < posts.length ? posts[index+1] : null
  const content = await readFile(filePath, 'utf-8')

  return {...post, content, next, prev}
}


