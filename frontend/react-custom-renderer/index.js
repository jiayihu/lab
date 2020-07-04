import React from 'react';
// import ReactDOM from "react-dom";
import { ReactSpeech } from './renderer/speech-renderer';

ReactSpeech.render([
  <alice key={1}>Benvenuti a Pordenone</alice>,
  <luca key={2}>Io sono Luca e farò il talk su React al posto suo</luca>,
]);
