import React from 'react'

import { theme, Button, Input} from '../components'

export default function Index() {
  return <>
    <main>
      <h2 style={{fontFamily: theme.fontPri, textAlign: 'center', fontSize: '4em'}}> Style Transfer</h2>
      <Form />
    </main>
  </>
}

function Form() {
  const [contentFile, setContentfile] = React.useState(null)
  const [styleFile, setStylefile] = React.useState(null)
  const [isAdvancedUpload, setIsAdvancedUpload] = React.useState(null)

  React.useEffect(() => {
    const div = document.createElement('div')
    const isAdvancedUpload = (('draggable' in div) || ('ondragstart' in div && 'ondrop' in div)) && 'FormData' in window && 'FileReader' in window
    setIsAdvancedUpload(isAdvancedUpload)
  }, [setIsAdvancedUpload])

  return <form
    onSubmit={(e) => {
      e.preventDefault()
      submitForm(e.target)
    }}
  >
    <style jsx>{`
      form {
        display: grid;
        justify-items: center;
        gap: 4rem;
      }

      .files {
        display: grid;
        grid-template-columns: 1fr 1fr;
        grid-gap: 2em;
        width: 100%;
      }


    `}</style>
    <div className='files'>
      <FileUploader isAdvancedUpload={isAdvancedUpload} label='Content Image' name='content-image' fileUpload={contentFile} setFileUpload={setContentfile} />
      <FileUploader isAdvancedUpload={isAdvancedUpload} label='Style Image' name='style-image' fileUpload={styleFile} setFileUpload={setStylefile} />
    </div>
    <Input type='email' name='email' label='Email' />
    { contentFile && styleFile
      ?  <Button type='submit'>Submit</Button>
      : <Button type='submit' disabled>Submit</Button>
    }
  </form>
}


function FileUploader({label, name, fileUpload, setFileUpload, isAdvancedUpload}) {
  const refs = {
    label: React.useRef(null),
    preview: React.useRef(null),
  }

  const handleFile = (e) => {
    const file = e.target.files[0]
    if (file.size > 10000000) alert('File size cannot exceed more than 10MB')
    setFileUpload(file)
  }

  React.useEffect(() => {
    const label = refs.label.current
    const preview = refs.preview.current
    if (!isAdvancedUpload || label === null || preview === null) return

    function addListenerMulti(el, s, fn) {
      s.split(' ').forEach(e => el.addEventListener(e, fn, false));
    }

    addListenerMulti(label, 'drag dragstart dragend dragover dragenter dragleave drop', (e) => {
      e.preventDefault();
      e.stopPropagation();
    })

    addListenerMulti(label, 'dragover dragenter', (e) => label.classList.add('is-dragover'))
    addListenerMulti(label, 'dragleave dragend drop', (e) => label.classList.remove('is-dragover'))

    label.addEventListener('drop', (e) => {
      const file = e.dataTransfer.files[0]
      setFileUpload(file)
    })

  }, [isAdvancedUpload, refs.label, setFileUpload])

  React.useEffect(() => {
    const preview = refs.preview.current
    if (preview === null || fileUpload === null || fileUpload === undefined || preview === undefined) return

    let oFReader = new FileReader()
    oFReader.readAsDataURL(fileUpload)
    oFReader.onload = function (oFREvent) {
      refs.preview.current.src = oFREvent.target.result
    }

  }, [fileUpload, refs.preview])

  return <div className='file-uploader'>
    <style jsx>{`
      .file-uploader {
        display: flex;
        flex-direction: column;
        align-items: center;
      }

      .label {
        background: black;
        color: ${theme.colPri};
        text-align: center;
        font-family: ${theme.fontPri};
        position: relative;
        padding: 0.5em 1em;
        font-size: 1.25em;
        width: 100%;
        color: white;
        margin-bottom: -4px;
      }

      .upload.box:focus::after { transform: translate(5px, 5px); }

      .upload {
        background: ${theme.colPri};
        position: relative;
        display: inline-block;
        width: 100%;
        height: 20em;
        display: inline-grid;
        place-items: center;
      }

      .upload__file { display: none; }
      .upload__img {
        width: 80%;
        max-height: 60%;
      }
      .upload__help { height: 80%; width: 80%; border: dashed black; position: absolute; display: flex; align-items: center; text-align: center; justify-content: center; }

    `}</style>
    <span className='label box'>{label}</span>
    <label className='box upload' ref={refs.label} tabIndex={0}>
      <input className='upload__file' name={name} type='file' onChange={handleFile} />
      <img className='upload__img' ref={refs.preview} src='' alt='' />
      { fileUpload
        ? null
        : <div className='upload__help'>
          Drop your photo here or <br/> click to select one from your computer.
        </div>
      }
    </label>
  </div>

}


function submitForm(form) {
  let formData = new FormData(form)
  fetch('/api/style', {method: 'post', body: formData})
    .then((res) => {
      window.location = `/styles?user=${formData.get('email')}`
    })
    .catch((err) => {})
}

