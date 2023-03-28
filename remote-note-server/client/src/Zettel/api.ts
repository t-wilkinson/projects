import { dispatch } from '../Events'
import { State } from '../App/store'

export type ZettelFile = { name: string; body: string }

const apiUrl = 'http://localhost:4000'

const getFile = async (state: State, name: string): Promise<ZettelFile> =>
  fetch(`${apiUrl}/zettels/${name}`, {
    method: 'GET',
  })
    .then(res => res.json())
    .then(res => {
      dispatch({ type: 'get-file', message: name, status: 'debug' })
      return res
    })

const getFiles = async (state: State) =>
  fetch(`${apiUrl}/zettels`, {
    method: 'GET',
  })
    .then(res => res.json())
    .then(res => {
      dispatch({ type: 'get-files', status: 'debug' })
      return res
    })

const saveFile = (state: State, name: string, body: string) =>
  fetch(`${apiUrl}/zettels/${name}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'text/plain',
    },
    body,
  }).then(() => {
    dispatch({ type: 'save-file', message: `Saved file ${name}`, status: 'success' })
  })

const createFile = (state: State, tag: string) =>
  fetch(`${apiUrl}/zettels`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ tag }),
  })
    .then(res => res.json())
    .then(res => {
      dispatch({
        type: 'create-file',
        message: `Create file with content ${res.filename}`,
        status: 'success',
      })
      return res.filename
    })

const deleteFile = (state: State, name: string) =>
  fetch(`${apiUrl}/zettels/${name}`, {
    method: 'DELETE',
  }).then(() => {
    dispatch({ type: 'delete-file', message: `Deleted file ${name}`, status: 'success' })
  })

export const api = {
  getFile,
  getFiles,
  saveFile,
  createFile,
  deleteFile,
}
