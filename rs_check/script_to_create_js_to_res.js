import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const sourceDirectory = path.join(__dirname, '../ps_lib/output');
const outputDirectory = path.join(__dirname, './src/lib_code'); 

if (!fs.existsSync(outputDirectory)) {
    console.log("I am here");
  fs.mkdirSync(outputDirectory, { recursive: true });
}
function processDirectory(directory) {
  fs.readdir(directory, { withFileTypes: true }, (err, files) => {
    console.log(directory)
    console.log(files)
    if (err) {
      console.error('Error reading directory:', directory, err);
      return;
    }

    files.forEach(file => {
      const fullPath = path.join(directory, file.name);
      console.log(fullPath)

      if (file.isDirectory()) {
        processDirectory(fullPath);
      } else if (file.name === 'index.js') {
        let folderName = path.basename(directory);
        const folderName_ = folderName;
        folderName = folderName.replace(/\./g, '_').toLowerCase();

        const resContent = createResContent(folderName, folderName_);
        folderName = folderName.charAt(0).toUpperCase() + folderName.slice(1);
        const resFilePath = path.join(outputDirectory, `${folderName}.res`);

        fs.writeFile(resFilePath, resContent, err => {
          if (err) {
            console.error('Error writing file:', resFilePath, err);
          } else {
            console.log(`Created: ${resFilePath}`);
          }
        });
      }
    });
  });
}

function createResContent(folderName, folderName_) {
  return `%%raw(\`import * as ${folderName} from "../../../ps_lib/output/${folderName_}/index.js"\`)
@val external ${folderName}Import: 'a = "${folderName}"

let ${folderName}Export = ${folderName}Import
`;
}

processDirectory(sourceDirectory);
