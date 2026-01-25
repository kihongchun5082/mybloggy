import { sendEmail } from '@/service/email'
import * as yup from 'yup'

const bodySchema = yup.object().shape({
  from: yup.string().email().required(),
  subject: yup.string().required(),
  message: yup.string().required(),
})

export async function POST(request:Request) {
  const body = await request.json()
  if(!bodySchema.isValidSync(body)) {
    return new Response(
      JSON.stringify({ message: '메일 전송에 실패하였습니다.' }),
      { status: 400 }
    );
  }
  //const { from, subject, message } = request.body
  // 받아온 데이터를 노드메일러를 이용해서 메일을 전송하면 됨
  return sendEmail(body).then(
    () =>
      new Response(JSON.stringify({ message: '메일을 성공적으로 보냈습니다' }), {
        status: 200,
      })
  ).catch(error => {
    console.error(error)
    return new Response(JSON.stringify({ message: '메일 전송에 실패하였습니다.' }), {
      status: 500,
    })
  })
}

