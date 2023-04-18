import cdsapi
c = cdsapi.Client()
years = range(1979, 2023) #this years from 1979 to 2022
for year in years:
    c.retrieve(
    'sis-agrometeorological-indicators',
        {
            'variable': 'precipitation_flux',
            'year': str(year),
            'month': [
            '09','10','11', '12'],
            #'month': ["09", "10", "11", "12"] #make a separate request for the remaining months by commenting on the one above
            'day': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
            '13', '14', '15',
            '16', '17', '18',
            '19', '22', '25',
            '28', '31', '20',
            '23', '26', '29',
            '21', '24','27',
            '30',
            ],
            'area': [
            -8.235, 21.88, -17.97, 33.49, #lat_max, lon_min, lat_min, lon_max
            ],
            'format': 'zip',
        },str(year)+'_sep_dec.zip') #change to str(year)+'_sep_dec.zip') when
        #sending request for remaining months.
