'use client';
import React from 'react';
import remarkGfm from 'remark-gfm'
import ReactMarkdown, { type Components } from 'react-markdown'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter';
import Image from 'next/image';
import { materialDark } from 'react-syntax-highlighter/dist/esm/styles/prism';

const components: Components = {
  code({className, children, ...props }) {
    const match = /language-(\w+)/.exec(className || '');
    return match ? (
      <SyntaxHighlighter
        language={match[1]}
        PreTag='div'
        // {...props}
        style={materialDark}
      >
        {String(children).replace(/\n$/, '')}
      </SyntaxHighlighter>
      ) : (
        <code className={className} {...props}>
          {children}
        </code>
      );
    },

  img({ src, alt }) {
    if (typeof src !== 'string' || !src) return null;
    return (
      <Image
        className='w-full max-h-none  object-cover'
        src={src ?? ''}
        alt={alt ?? ''}
        width={500}
        height={350}
      />
    );
  },
};

export default function MarkdownViewer({ content }: {content: string}) {
  return (
    <div className = 'prose max-w-none'>
      <ReactMarkdown remarkPlugins={[ remarkGfm ]}
 components={ components }>
      {content}
    </ReactMarkdown>
    </div>
  );
}

