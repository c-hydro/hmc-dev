% Function to analyze and plot 2d variable(s) used in Continuum Model
function oStruct = debug_2dVar(sFilePath)
    
    % MATLAB:
    % COMMAND LINE (SAME FOLDER):   matlab -nosplash -nodesktop -r "debug_2dVar()"
    % COMMAND LINE (OTHER FOLDER):  matlab -nosplash -nodesktop -r "debug_2dVar('/home/fabio/NetBeansProjects/Fortran_Continuum_RegMarche/')"
    
    % FORTRAN:
    % COMMAND LINE (CODE SIDE): call debug_2dVar(dble(a2dVarData), iRows, iCols, 1)
    
    
    disp('----------------------------------------------------------------')
    disp('Script to analyze 2dVar data used in Continuum model')
    disp('Vers. 1.0.2 - 20151012')
    disp('----------------------------------------------------------------')
    disp('')
    
    clearvars -except sFilePath
    
    if (nargin < 1)
        sCurrentFolder = pwd;
        sFilePath = [sCurrentFolder,'/'];
        disp(['File(s) are available in the SAME folder (',sFilePath,')'])
    else
        disp(['File are available in ANOTHER folder (',sFilePath,')'])
    end
    
    % Time definition
    sTime = datestr(now); sTime = strrep(sTime, ' ', '_');
    disp(['TIME ANALYSIS DEFINITION: ',sTime])
    
    % Step question
    sAnswer = ''; iTest = [];
    while isempty(iTest)
        sQuestion =  'Start Debug (d); Close debug (q)';
        sAnswer = input(sQuestion,'s');
        disp([' ------> ', sAnswer])
        % Check answer type
        iTest = findstr(sAnswer, 'dq');
        if isempty(iTest)
            disp('Check your selection! Type is wrong!')
        end  
        
    end

    if (sAnswer == 'd')
         
         % Step question
        sAnswerNumber = ''; iTest = [];
        while isempty(iTest)
            sQuestion =  'Starting 2dVar ("int"); Default 2dVar (1); Close debug (q)';
            sAnswerNumber = input(sQuestion,'s');
            disp([' ------> ', sAnswer])
            
            if sAnswerNumber == 'q'
                disp('QUIT ... Bye Bye')
                disp('----------------------------------------------------------------')
                quit
            else
                % Check answer type
                iTest = findstr(sAnswerNumber, '0123456789q');
                if isempty(iTest)
                    disp('Check your selection! Type is wrong!')
                else
                    iN = str2num(sAnswerNumber);
                end
            end
        end
        
        %iN = 3;  % index to test one selected file (for example 3)
        oStruct = struct('date', [sTime]);
        sAnswer = 'n';
    
        while sAnswer == 'n' || sAnswer == 'i' || sAnswer == 'o'
               
            sN = num2str(iN,'%02.0f');

            sFileName = [sFilePath,'data_file_',sN,'.txt'];
            disp(['Analyze and plot data file: ',sFileName])

            if exist(sFileName, 'file')

                disp('File available into selected folder')

                %a2dDataRaw = importdata(sFileName); --> a volte legge male
                %i file con troppa approssimazione e-10, e-17 ...
                a2dDataRaw = load(sFileName);
                iCols = nanmax(a2dDataRaw(:,1)); iRows = nanmax(a2dDataRaw(:,2));
                
                if iCols*iRows ~= size(a2dDataRaw,1)
                    disp('ATTENTION: PROBLEM TO LOAD FILE CORRECTLY!!')
                    disp(' ------> FILE VALUES COULD BE READ UNCORRECTLY! --> EXIT DEBUG!!! ')
                    break
                else
                    disp('FILE LOADED CORRECTLY!')
                end
                
                % Check to value error
                a1iIndex = [];
                a1iIndex = find(isnan(a2dDataRaw(:,3)));
                
                if ~isempty(a1iIndex)
                    
                    a1sIndex = '';
                    a1sIndex = strtrim(cellstr(num2str(a1iIndex'))');
                    
                    disp('ATTENTION: SOME VALUES ARE CORRECTLY LOADED (E-106 values ~ 0)')
                    disp([' ------> UNKWNOWN FORMAT INDEXES: ', char(a1sIndex)])
                    a2dDataRaw(a1iIndex, :) = [];
                    a1iIndex = a1iIndex - 1;
                    a2dDataRaw(a1iIndex, :) = 0;
                    disp(' ------> UNKWNOWN FORMAT INDEXES INITIALIZE = 0')
                
                else
                    disp('ALL VALUES ARE CORRECTLY LOADED')
                end
                
                a2dDataValue = rot90(reshape(a2dDataRaw(:,3), iCols, iRows));

                a2dDataValue(a2dDataValue==-9999) = NaN;
                a2dDataValue(a2dDataValue==-999.9) = NaN;

                dDataValueMin = nanmin(nanmin(a2dDataValue));
                dDataValueMax = nanmax(nanmax(a2dDataValue));
                dDataValueMean = nanmean(nanmean(a2dDataValue));

                disp(['VarMin: ',num2str(dDataValueMin),' - VarMax: ',num2str(dDataValueMax),' - VarMean: ',num2str(dDataValueMean)])

                sFileName4Graph = strrep(sFileName, '_', '\_');
                sTimeGraph = strrep(sTime,'_', '\_');

                % Save all field(s) to store value(s)
                oStruct = setfield(oStruct, ['data_file_',sN], a2dDataValue);

                figure(iN)
                imagesc(a2dDataValue); colorbar();
                title({['Filename: ',sFileName4Graph], ...
                    ['VarMin: ',num2str(dDataValueMin),' - VarMax: ',num2str(dDataValueMax),' VarMean: ',num2str(dDataValueMean)],...
                    ['Time: ',sTimeGraph]})

            else
                disp('File not found into selected folder!')           
            end

            % Step question
            sAnswer = ''; iTest = [];
            while isempty(iTest)
                sQuestion =  'Close debug (q); Analyze another 2dVar (n); Analyze same 2dvar (o); Save figures (s); Close all figures (c); Re-init analysis (i); Return to matlab (r)';
                sAnswer = input(sQuestion,'s');
                disp([' ------> ', sAnswer])
                % Check answer type
                iTest = findstr(sAnswer, 'qnsciro');
                if isempty(iTest)
                    disp('Check your selection! Type is wrong!')
                end  
            end 

            % Save figure(s)
            if sAnswer == 's'

                oFig = get(0,'children');
                for iFig = 1:length(oFig)

                    sFileNameFig = ['data_file_figure_',num2str(iFig),'_',sTime];

                    disp(['Saving: ', sFileNameFig, ' ... '])
                    saveas(oFig(iFig), sFileNameFig, 'png');
                    close(oFig(iFig))
                    disp(['Saving: ', sFileNameFig, ' ... OK'])
                end

                disp('Save all figures ... OK')
                disp('----------------------------------------------------------------')

                % Step question
                sQuestion =  'Close debug (q); Analyze another 2dVar (n); Analyze same 2dvar (o); Re-init analysis (i); Return to matlab (r)';
                sAnswer = input(sQuestion,'s');
                disp([' ------> ', sAnswer])
            end

            % Close all figure(s)
            if sAnswer == 'c'
                disp('Close all figures ... OK')
                disp('----------------------------------------------------------------')
                close all

                % Step question
                sQuestion =  'Close debug (q); Analyze another 2dVar (n); Analyze same 2dvar (o); Re-init analysis (i); Return to matlab (r)';
                sAnswer = input(sQuestion,'s');
                disp([' ------> ', sAnswer])
            end

            % Exit code(s)
            if sAnswer == 'q'
                disp('QUIT ... Bye Bye')
                disp('----------------------------------------------------------------')
                quit
            elseif sAnswer == 'n'
                disp('Analyse another 2dVar ... OK')
                disp('----------------------------------------------------------------')
                iN = iN + 1;
            elseif sAnswer == 'i'
                disp('Re-init analysis ... OK')
                disp('----------------------------------------------------------------')
                close all
                clearvars -except sFilePath sAnswer
                sTime = datestr(now); sTime = strrep(sTime, ' ', '_');
                disp(['TIME ANALYSIS DEFINITION: ',sTime])
                oStruct = struct('date', [sTime]);
                iN = 1;
            elseif sAnswer == 'o'
                disp('Re-analyse same 2dVar ... OK')
                disp('----------------------------------------------------------------')
                
                oFig = get(0,'children');
                close(oFig(length(oFig)))
 
                clearvars -except sFilePath sAnswer oStruct sTime iN
                %sTime = datestr(now); sTime = strrep(sTime, ' ', '_');
                %disp(['TIME ANALYSIS DEFINITION: ',sTime])
                %oStruct = struct('date', [sTime]);
                
                iN = iN;
             
            elseif sAnswer == '+'
                disp('Re-analyse 2dVar + 1... OK')
                disp('----------------------------------------------------------------')
                
                oFig = get(0,'children');
                close(oFig(length(oFig)))
 
                clearvars -except sFilePath sAnswer oStruct sTime iN
                %sTime = datestr(now); sTime = strrep(sTime, ' ', '_');
                %disp(['TIME ANALYSIS DEFINITION: ',sTime])
                %oStruct = struct('date', [sTime]);
                
                iN = iN;
                
            elseif sAnswer == 'r'
                disp('Return to Matlab ... OK')
                disp('NOW ALL DATA ARE AVAILABLE ON MATLAB WORKSPACE')
                disp('----------------------------------------------------------------')
                return
            end

        end
    
    elseif sAnswer == 'q'
        disp('QUIT ... Bye Bye')
        disp('----------------------------------------------------------------')
        quit
    end
    





